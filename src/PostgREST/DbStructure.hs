{-|
Module      : PostgREST.DbStructure
Description : PostgREST schema cache

This module contains queries that target PostgreSQL system catalogs, these are used to build the schema cache(DbStructure).

The schema cache is necessary for resource embedding, foreign keys are used for inferring the relationships between tables.

These queries are executed once at startup or when PostgREST is reloaded.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module PostgREST.DbStructure (
  getDbStructure
, accessibleTables
, accessibleProcs
, schemaDescription
, getPgVersion
) where

import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Hasql.Decoders      as HD
import qualified Hasql.Encoders      as HE
import qualified Hasql.Session       as H
import qualified Hasql.Statement     as H
import qualified Hasql.Transaction   as HT

import Data.Set                      as S (fromList)
import Data.Text                     (breakOn, dropAround, split,
                                      splitOn, strip)
import GHC.Exts                      (groupWith)
import Protolude                     hiding (toS)
import Protolude.Conv                (toS)
import Protolude.Unsafe              (unsafeHead)
import Text.InterpolatedString.Perl6 (q, qc)

import PostgREST.Private.Common
import PostgREST.Types

getDbStructure :: [Schema] -> PgVersion -> HT.Transaction DbStructure
getDbStructure schemas pgVer = do
  HT.sql "set local schema ''" -- This voids the search path. The following queries need this for getting the fully qualified name(schema.name) of every db object
  tabs    <- HT.statement () allTables
  cols    <- HT.statement schemas $ allColumns tabs
  srcCols <- HT.statement schemas $ allSourceColumns cols pgVer
  m2oRels <- HT.statement () $ allM2ORels tabs cols
  keys    <- HT.statement () $ allPrimaryKeys tabs
  procs   <- HT.statement schemas allProcs

  let rels = addM2MRels . addO2MRels $ addViewM2ORels srcCols m2oRels
      cols' = addForeignKeys rels cols
      keys' = addViewPrimaryKeys srcCols keys

  return DbStructure {
      dbTables = tabs
    , dbColumns = cols'
    , dbRelations = rels
    , dbPrimaryKeys = keys'
    , dbProcs = procs
    , pgVersion = pgVer
    }

decodeTables :: HD.Result [Table]
decodeTables =
  HD.rowList tblRow
 where
  tblRow = Table <$> column HD.text
                 <*> column HD.text
                 <*> nullableColumn HD.text
                 <*> column HD.bool

decodeColumns :: [Table] -> HD.Result [Column]
decodeColumns tables =
  mapMaybe (columnFromRow tables) <$> HD.rowList colRow
 where
  colRow =
    (,,,,,,,,,,,)
      <$> column HD.text <*> column HD.text
      <*> column HD.text <*> nullableColumn HD.text
      <*> column HD.int4 <*> column HD.bool
      <*> column HD.text <*> column HD.bool
      <*> nullableColumn HD.int4
      <*> nullableColumn HD.int4
      <*> nullableColumn HD.text
      <*> nullableColumn HD.text

decodeRels :: [Table] -> [Column] -> HD.Result [Relation]
decodeRels tables cols =
  mapMaybe (relFromRow tables cols) <$> HD.rowList relRow
 where
  relRow = (,,,,,,)
    <$> column HD.text
    <*> column HD.text
    <*> column HD.text
    <*> arrayColumn HD.text
    <*> column HD.text
    <*> column HD.text
    <*> arrayColumn HD.text

decodePks :: [Table] -> HD.Result [PrimaryKey]
decodePks tables =
  mapMaybe (pkFromRow tables) <$> HD.rowList pkRow
 where
  pkRow = (,,) <$> column HD.text <*> column HD.text <*> column HD.text

decodeSourceColumns :: [Column] -> HD.Result [SourceColumn]
decodeSourceColumns cols =
  mapMaybe (sourceColumnFromRow cols) <$> HD.rowList srcColRow
 where
  srcColRow = (,,,,,)
    <$> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text
    <*> column HD.text <*> column HD.text

sourceColumnFromRow :: [Column] -> (Text,Text,Text,Text,Text,Text) -> Maybe SourceColumn
sourceColumnFromRow allCols (s1,t1,c1,s2,t2,c2) = (,) <$> col1 <*> col2
  where
    col1 = findCol s1 t1 c1
    col2 = findCol s2 t2 c2
    findCol s t c = find (\col -> (tableSchema . colTable) col == s && (tableName . colTable) col == t && colName col == c) allCols

decodeProcs :: HD.Result ProcsMap
decodeProcs =
  -- Duplicate rows for a function means they're overloaded, order these by least args according to ProcDescription Ord instance
  map sort . M.fromListWith (++) . map ((\(x,y) -> (x, [y])) . addKey) <$> HD.rowList procRow
  where
    procRow = ProcDescription
              <$> column HD.text
              <*> column HD.text
              <*> nullableColumn HD.text
              <*> (parseArgs <$> column HD.text)
              <*> (parseRetType
                  <$> column HD.text
                  <*> column HD.text
                  <*> column HD.bool
                  <*> column HD.char)
              <*> (parseVolatility <$> column HD.char)
              <*> (parseAccept <$> nullableArrayColumn HD.text)

    addKey :: ProcDescription -> (QualifiedIdentifier, ProcDescription)
    addKey pd = (QualifiedIdentifier (pdSchema pd) (pdName pd), pd)

    parseArgs :: Text -> [PgArg]
    parseArgs = mapMaybe parseArg . filter (not . isPrefixOf "OUT" . toS) . map strip . split (==',')

    parseArg :: Text -> Maybe PgArg
    parseArg a =
      let arg = lastDef "" $ splitOn "INOUT " a
          (body, def) = breakOn " DEFAULT " arg
          (name, typ) = breakOn " " body in
      if T.null typ
         then Nothing
         else Just $
           PgArg (dropAround (== '"') name) (strip typ) (T.null def)

    parseRetType :: Text -> Text -> Bool -> Char -> RetType
    parseRetType schema name isSetOf typ
      | isSetOf   = SetOf pgType
      | otherwise = Single pgType
      where
        qi = QualifiedIdentifier schema name
        pgType = case typ of
          'c' -> Composite qi
          'p' -> if name == "record" -- Only pg pseudo type that is a row type is 'record'
                   then Composite qi
                   else Scalar qi
          _   -> Scalar qi -- 'b'ase, 'd'omain, 'e'num, 'r'ange

    parseVolatility :: Char -> ProcVolatility
    parseVolatility v | v == 'i' = Immutable
                      | v == 's' = Stable
                      | otherwise = Volatile -- only 'v' can happen here

    parseAccept :: Maybe [Text] -> Maybe Text
    parseAccept confs = T.stripPrefix "pgrst.accept=" =<< find ("pgrst.accept" `T.isPrefixOf`) (fromMaybe [] confs)

allProcs :: H.Statement [Schema] ProcsMap
allProcs = H.Statement (toS sql) (arrayParam HE.text) decodeProcs True
  where
    sql = procsSqlQuery <> " WHERE pn.nspname = ANY($1)"

accessibleProcs :: H.Statement Schema ProcsMap
accessibleProcs = H.Statement (toS sql) (param HE.text) decodeProcs True
  where
    sql = procsSqlQuery <> " WHERE pn.nspname = $1 AND has_function_privilege(p.oid, 'execute')"

procsSqlQuery :: SqlQuery
procsSqlQuery = [q|
  SELECT
    pn.nspname as proc_schema,
    p.proname as proc_name,
    d.description as proc_description,
    pg_get_function_arguments(p.oid) as args,
    tn.nspname as rettype_schema,
    coalesce(comp.relname, t.typname) as rettype_name,
    p.proretset as rettype_is_setof,
    t.typtype as rettype_typ,
    p.provolatile,
    p.proconfig as proc_config
  FROM pg_proc p
    JOIN pg_namespace pn ON pn.oid = p.pronamespace
    JOIN pg_type t ON t.oid = p.prorettype
    JOIN pg_namespace tn ON tn.oid = t.typnamespace
    LEFT JOIN pg_class comp ON comp.oid = t.typrelid
    LEFT JOIN pg_catalog.pg_description as d on d.objoid = p.oid
|]

schemaDescription :: H.Statement Schema (Maybe Text)
schemaDescription =
    H.Statement sql (param HE.text) (join <$> HD.rowMaybe (nullableColumn HD.text)) True
  where
    sql = [q|
      select
        description
      from
        pg_catalog.pg_namespace n
        left join pg_catalog.pg_description d on d.objoid = n.oid
      where
        n.nspname = $1 |]

accessibleTables :: H.Statement Schema [Table]
accessibleTables =
  H.Statement sql (param HE.text) decodeTables True
 where
  sql = [q|
    select
      n.nspname as table_schema,
      relname as table_name,
      d.description as table_description,
      (
        c.relkind in ('r', 'v', 'f')
        and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
        -- The function `pg_relation_is_updateable` returns a bitmask where 8
        -- corresponds to `1 << CMD_INSERT` in the PostgreSQL source code, i.e.
        -- it's possible to insert into the relation.
        or (exists (
          select 1
          from pg_trigger
          where
            pg_trigger.tgrelid = c.oid
            and (pg_trigger.tgtype::integer & 69) = 69)
            -- The trigger type `tgtype` is a bitmask where 69 corresponds to
            -- TRIGGER_TYPE_ROW + TRIGGER_TYPE_INSTEAD + TRIGGER_TYPE_INSERT
            -- in the PostgreSQL source code.
        )
      ) as insertable
    from
      pg_class c
      join pg_namespace n on n.oid = c.relnamespace
      left join pg_catalog.pg_description as d on d.objoid = c.oid and d.objsubid = 0
    where
      c.relkind in ('v', 'r', 'm', 'f')
      and n.nspname = $1
      and (
        pg_has_role(c.relowner, 'USAGE')
        or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
        or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
      )
    order by relname |]

addForeignKeys :: [Relation] -> [Column] -> [Column]
addForeignKeys rels = map addFk
  where
    addFk col = col { colFK = fk col }
    fk col = find (lookupFn col) rels >>= relToFk col
    lookupFn :: Column -> Relation -> Bool
    lookupFn c Relation{relColumns=cs, relType=rty} = c `elem` cs && rty==M2O
    relToFk col Relation{relColumns=cols, relFColumns=colsF} = do
      pos <- L.elemIndex col cols
      colF <- atMay colsF pos
      return $ ForeignKey colF

{-
Adds Views M2O Relations based on SourceColumns found, the logic is as follows:

Having a Relation{relTable=t1, relColumns=[c1], relFTable=t2, relFColumns=[c2], relType=M2O} represented by:

t1.c1------t2.c2

When only having a t1_view.c1 source column, we need to add a View-Table M2O Relation

         t1.c1----t2.c2         t1.c1----------t2.c2
                         ->            ________/
                                      /
      t1_view.c1             t1_view.c1


When only having a t2_view.c2 source column, we need to add a Table-View M2O Relation

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________
                                                    \
                    t2_view.c2                      t2_view.c1

When having t1_view.c1 and a t2_view.c2 source columns, we need to add a View-View M2O Relation in addition to the prior

         t1.c1----t2.c2               t1.c1----------t2.c2
                               ->          \________/
                                           /        \
    t1_view.c1     t2_view.c2     t1_view.c1-------t2_view.c1

The logic for composite pks is similar just need to make sure all the Relation columns have source columns.
-}
addViewM2ORels :: [SourceColumn] -> [Relation] -> [Relation]
addViewM2ORels allSrcCols = concatMap (\rel ->
  rel : case rel of
    Relation{relType=M2O, relTable, relColumns, relConstraint, relFTable, relFColumns} ->

      let srcColsGroupedByView :: [Column] -> [[SourceColumn]]
          srcColsGroupedByView relCols = L.groupBy (\(_, viewCol1) (_, viewCol2) -> colTable viewCol1 == colTable viewCol2) $
                                         filter (\(c, _) -> c `elem` relCols) allSrcCols
          relSrcCols = srcColsGroupedByView relColumns
          relFSrcCols = srcColsGroupedByView relFColumns
          getView :: [SourceColumn] -> Table
          getView = colTable . snd . unsafeHead
          srcCols `allSrcColsOf` cols = S.fromList (fst <$> srcCols) == S.fromList cols
          -- Relation is dependent on the order of relColumns and relFColumns to get the join conditions right in the generated query.
          -- So we need to change the order of the SourceColumns to match the relColumns
          -- TODO: This could be avoided if the Relation type is improved with a structure that maintains the association of relColumns and relFColumns
          srcCols `sortAccordingTo` cols = sortOn (\(k, _) -> L.lookup k $ zip cols [0::Int ..]) srcCols

          viewTableM2O =
            [ Relation (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
                       relConstraint relFTable relFColumns
                       M2O Nothing
            | srcCols <- relSrcCols, srcCols `allSrcColsOf` relColumns ]

          tableViewM2O =
            [ Relation relTable relColumns
                       relConstraint
                       (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relFColumns)
                       M2O Nothing
            | fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relFColumns ]

          viewViewM2O =
            [ Relation (getView srcCols) (snd <$> srcCols `sortAccordingTo` relColumns)
                       relConstraint
                       (getView fSrcCols) (snd <$> fSrcCols `sortAccordingTo` relFColumns)
                       M2O Nothing
            | srcCols  <- relSrcCols, srcCols `allSrcColsOf` relColumns
            , fSrcCols <- relFSrcCols, fSrcCols `allSrcColsOf` relFColumns ]

      in viewTableM2O ++ tableViewM2O ++ viewViewM2O

    _ -> [])

addO2MRels :: [Relation] -> [Relation]
addO2MRels = concatMap (\rel@(Relation t c cn ft fc _ _) -> [rel, Relation ft fc cn t c O2M Nothing])

addM2MRels :: [Relation] -> [Relation]
addM2MRels rels = rels ++ addMirrorRel (mapMaybe junction2Rel junctions)
  where
    junctions = join $ map (combinations 2) $ filter (not . null) $ groupWith groupFn $ filter ( (==M2O). relType) rels
    groupFn :: Relation -> Text
    groupFn Relation{relTable=Table{tableSchema=s, tableName=t}} = s <> "_" <> t
    -- Reference : https://wiki.haskell.org/99_questions/Solutions/26
    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [ [] ]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                               , ys <- combinations (n-1) xs']
    junction2Rel [
      Relation{relTable=jt, relColumns=jc1, relConstraint=const1, relFTable=t,  relFColumns=c},
      Relation{             relColumns=jc2, relConstraint=const2, relFTable=ft, relFColumns=fc}
      ]
      | jc1 /= jc2 && length jc1 == 1 && length jc2 == 1 = Just $ Relation t c Nothing ft fc M2M (Just $ Junction jt const1 jc1 const2 jc2)
      | otherwise = Nothing
    junction2Rel _ = Nothing
    addMirrorRel = concatMap (\rel@(Relation t c _ ft fc _ (Just (Junction jt const1 jc1 const2 jc2))) ->
      [rel, Relation ft fc Nothing t c M2M (Just (Junction jt const2 jc2 const1 jc1))])

addViewPrimaryKeys :: [SourceColumn] -> [PrimaryKey] -> [PrimaryKey]
addViewPrimaryKeys srcCols = concatMap (\pk ->
  let viewPks = (\(_, viewCol) -> PrimaryKey{pkTable=colTable viewCol, pkName=colName viewCol}) <$>
                filter (\(col, _) -> colTable col == pkTable pk && colName col == pkName pk) srcCols in
  pk : viewPks)

allTables :: H.Statement () [Table]
allTables =
  H.Statement sql HE.noParams decodeTables True
 where
  sql = [q|
    SELECT
      n.nspname AS table_schema,
      c.relname AS table_name,
      NULL AS table_description,
      (
        c.relkind IN ('r', 'v','f')
        AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
        OR EXISTS (
          SELECT 1
          FROM pg_trigger
          WHERE
            pg_trigger.tgrelid = c.oid
            AND (pg_trigger.tgtype::integer & 69) = 69
        )
      ) AS insertable
    FROM pg_class c
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE c.relkind IN ('v','r','m','f')
      AND n.nspname NOT IN ('pg_catalog', 'information_schema')
    GROUP BY table_schema, table_name, insertable
    ORDER BY table_schema, table_name |]

allColumns :: [Table] -> H.Statement [Schema] [Column]
allColumns tabs =
 H.Statement sql (arrayParam HE.text) (decodeColumns tabs) True
 where
  sql = [q|
    SELECT DISTINCT
        info.table_schema AS schema,
        info.table_name AS table_name,
        info.column_name AS name,
        info.description AS description,
        info.ordinal_position AS position,
        info.is_nullable::boolean AS nullable,
        info.data_type AS col_type,
        info.is_updatable::boolean AS updatable,
        info.character_maximum_length AS max_len,
        info.numeric_precision AS precision,
        info.column_default AS default_value,
        array_to_string(enum_info.vals, ',') AS enum
    FROM (
        -- CTE based on pg_catalog to get PRIMARY/FOREIGN key and UNIQUE columns outside api schema
        WITH key_columns AS (
             SELECT
               r.oid AS r_oid,
               c.oid AS c_oid,
               n.nspname,
               c.relname,
               r.conname,
               r.contype,
               unnest(r.conkey) AS conkey
             FROM
               pg_catalog.pg_constraint r,
               pg_catalog.pg_class c,
               pg_catalog.pg_namespace n
             WHERE
               r.contype IN ('f', 'p', 'u')
               AND c.relkind IN ('r', 'v', 'f', 'm')
               AND r.conrelid = c.oid
               AND c.relnamespace = n.oid
               AND n.nspname <> ANY (ARRAY['pg_catalog', 'information_schema'] || $1)
        ),
        /*
        -- CTE based on information_schema.columns
        -- changed:
        -- remove the owner filter
        -- limit columns to the ones in the api schema or PK/FK columns
        */
        columns AS (
            SELECT
                nc.nspname::name AS table_schema,
                c.relname::name AS table_name,
                a.attname::name AS column_name,
                d.description AS description,
                a.attnum::integer AS ordinal_position,
                pg_get_expr(ad.adbin, ad.adrelid)::text AS column_default,
                not (a.attnotnull OR t.typtype = 'd' AND t.typnotnull) AS is_nullable,
                    CASE
                        WHEN t.typtype = 'd' THEN
                        CASE
                            WHEN bt.typelem <> 0::oid AND bt.typlen = (-1) THEN 'ARRAY'::text
                            WHEN nbt.nspname = 'pg_catalog'::name THEN format_type(t.typbasetype, NULL::integer)
                            ELSE format_type(a.atttypid, a.atttypmod)
                        END
                        ELSE
                        CASE
                            WHEN t.typelem <> 0::oid AND t.typlen = (-1) THEN 'ARRAY'::text
                            WHEN nt.nspname = 'pg_catalog'::name THEN format_type(a.atttypid, NULL::integer)
                            ELSE format_type(a.atttypid, a.atttypmod)
                        END
                    END::text AS data_type,
                information_schema._pg_char_max_length(
                    information_schema._pg_truetypid(a.*, t.*),
                    information_schema._pg_truetypmod(a.*, t.*)
                )::integer AS character_maximum_length,
                information_schema._pg_numeric_precision(
                    information_schema._pg_truetypid(a.*, t.*),
                    information_schema._pg_truetypmod(a.*, t.*)
                )::integer AS numeric_precision,
                COALESCE(bt.typname, t.typname)::name AS udt_name,
                (
                    c.relkind in ('r', 'v', 'f')
                    AND pg_column_is_updatable(c.oid::regclass, a.attnum, false)
                )::bool is_updatable
            FROM pg_attribute a
                LEFT JOIN key_columns kc
                    ON kc.conkey = a.attnum AND kc.c_oid = a.attrelid
                LEFT JOIN pg_catalog.pg_description AS d
                    ON d.objoid = a.attrelid and d.objsubid = a.attnum
                LEFT JOIN pg_attrdef ad
                    ON a.attrelid = ad.adrelid AND a.attnum = ad.adnum
                JOIN (pg_class c JOIN pg_namespace nc ON c.relnamespace = nc.oid)
                    ON a.attrelid = c.oid
                JOIN (pg_type t JOIN pg_namespace nt ON t.typnamespace = nt.oid)
                    ON a.atttypid = t.oid
                LEFT JOIN (pg_type bt JOIN pg_namespace nbt ON bt.typnamespace = nbt.oid)
                    ON t.typtype = 'd' AND t.typbasetype = bt.oid
                LEFT JOIN (pg_collation co JOIN pg_namespace nco ON co.collnamespace = nco.oid)
                    ON a.attcollation = co.oid AND (nco.nspname <> 'pg_catalog'::name OR co.collname <> 'default'::name)
            WHERE
                NOT pg_is_other_temp_schema(nc.oid)
                AND a.attnum > 0
                AND NOT a.attisdropped
                AND c.relkind in ('r', 'v', 'f', 'm')
                -- Filter only columns that are FK/PK or in the api schema:
                AND (nc.nspname = ANY ($1) OR kc.r_oid IS NOT NULL)
        )
        SELECT
            table_schema,
            table_name,
            column_name,
            description,
            ordinal_position,
            is_nullable,
            data_type,
            is_updatable,
            character_maximum_length,
            numeric_precision,
            column_default,
            udt_name
        FROM columns
        WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
    ) AS info
    LEFT OUTER JOIN (
        SELECT
            n.nspname AS s,
            t.typname AS n,
            array_agg(e.enumlabel ORDER BY e.enumsortorder) AS vals
        FROM pg_type t
        JOIN pg_enum e ON t.oid = e.enumtypid
        JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
        GROUP BY s,n
    ) AS enum_info ON (info.udt_name = enum_info.n)
    ORDER BY schema, position |]

columnFromRow :: [Table] ->
                 (Text,        Text,        Text,
                  Maybe Text,  Int32,       Bool,
                  Text,        Bool,        Maybe Int32,
                  Maybe Int32, Maybe Text,  Maybe Text)
                 -> Maybe Column
columnFromRow tabs (s, t, n, desc, pos, nul, typ, u, l, p, d, e) = buildColumn <$> table
  where
    buildColumn tbl = Column tbl n desc pos nul typ u l p d (parseEnum e) Nothing
    table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs
    parseEnum :: Maybe Text -> [Text]
    parseEnum = maybe [] (split (==','))

allM2ORels :: [Table] -> [Column] -> H.Statement () [Relation]
allM2ORels tabs cols =
  H.Statement sql HE.noParams (decodeRels tabs cols) True
 where
  sql = [q|
    SELECT ns1.nspname AS table_schema,
           tab.relname AS table_name,
           conname     AS constraint_name,
           column_info.cols AS columns,
           ns2.nspname AS foreign_table_schema,
           other.relname AS foreign_table_name,
           column_info.refs AS foreign_columns
    FROM pg_constraint,
    LATERAL (
      SELECT array_agg(cols.attname) AS cols,
                    array_agg(cols.attnum)  AS nums,
                    array_agg(refs.attname) AS refs
      FROM ( SELECT unnest(conkey) AS col, unnest(confkey) AS ref) k,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = conrelid AND attnum = col) AS cols,
      LATERAL (SELECT * FROM pg_attribute WHERE attrelid = confrelid AND attnum = ref) AS refs) AS column_info,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = connamespace) AS ns1,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = conrelid) AS tab,
    LATERAL (SELECT * FROM pg_class WHERE pg_class.oid = confrelid) AS other,
    LATERAL (SELECT * FROM pg_namespace WHERE pg_namespace.oid = other.relnamespace) AS ns2
    WHERE confrelid != 0
    ORDER BY (conrelid, column_info.nums) |]

relFromRow :: [Table] -> [Column] -> (Text, Text, Text, [Text], Text, Text, [Text]) -> Maybe Relation
relFromRow allTabs allCols (rs, rt, cn, rcs, frs, frt, frcs) =
  Relation <$> table <*> cols <*> pure (Just cn) <*> tableF <*> colsF <*> pure M2O <*> pure Nothing
  where
    findTable s t = find (\tbl -> tableSchema tbl == s && tableName tbl == t) allTabs
    findCol s t c = find (\col -> tableSchema (colTable col) == s && tableName (colTable col) == t && colName col == c) allCols
    table  = findTable rs rt
    tableF = findTable frs frt
    cols  = mapM (findCol rs rt) rcs
    colsF = mapM (findCol frs frt) frcs

allPrimaryKeys :: [Table] -> H.Statement () [PrimaryKey]
allPrimaryKeys tabs =
  H.Statement sql HE.noParams (decodePks tabs) True
 where
  sql = [q|
    -- CTE to replace information_schema.table_constraints to remove owner limit
    WITH tc AS (
        SELECT
            c.conname::name AS constraint_name,
            nr.nspname::name AS table_schema,
            r.relname::name AS table_name
        FROM pg_namespace nc,
            pg_namespace nr,
            pg_constraint c,
            pg_class r
        WHERE
            nc.oid = c.connamespace
            AND nr.oid = r.relnamespace
            AND c.conrelid = r.oid
            AND r.relkind = 'r'
            AND NOT pg_is_other_temp_schema(nr.oid)
            AND c.contype = 'p'
    ),
    -- CTE to replace information_schema.key_column_usage to remove owner limit
    kc AS (
        SELECT
            ss.conname::name AS constraint_name,
            ss.nr_nspname::name AS table_schema,
            ss.relname::name AS table_name,
            a.attname::name AS column_name,
            (ss.x).n::integer AS ordinal_position,
            CASE
                WHEN ss.contype = 'f' THEN information_schema._pg_index_position(ss.conindid, ss.confkey[(ss.x).n])
                ELSE NULL::integer
            END::integer AS position_in_unique_constraint
        FROM pg_attribute a,
            ( SELECT r.oid AS roid,
                r.relname,
                r.relowner,
                nc.nspname AS nc_nspname,
                nr.nspname AS nr_nspname,
                c.oid AS coid,
                c.conname,
                c.contype,
                c.conindid,
                c.confkey,
                information_schema._pg_expandarray(c.conkey) AS x
               FROM pg_namespace nr,
                pg_class r,
                pg_namespace nc,
                pg_constraint c
              WHERE
                nr.oid = r.relnamespace
                AND r.oid = c.conrelid
                AND nc.oid = c.connamespace
                AND c.contype in ('p', 'u', 'f')
                AND r.relkind = 'r'
                AND NOT pg_is_other_temp_schema(nr.oid)
            ) ss
        WHERE
          ss.roid = a.attrelid
          AND a.attnum = (ss.x).x
          AND NOT a.attisdropped
    )
    SELECT
        kc.table_schema,
        kc.table_name,
        kc.column_name
    FROM
        tc, kc
    WHERE
        kc.table_name = tc.table_name AND
        kc.table_schema = tc.table_schema AND
        kc.constraint_name = tc.constraint_name AND
        kc.table_schema NOT IN ('pg_catalog', 'information_schema') |]

pkFromRow :: [Table] -> (Schema, Text, Text) -> Maybe PrimaryKey
pkFromRow tabs (s, t, n) = PrimaryKey <$> table <*> pure n
  where table = find (\tbl -> tableSchema tbl == s && tableName tbl == t) tabs

allSourceColumns :: [Column] -> PgVersion -> H.Statement [Schema] [SourceColumn]
allSourceColumns cols pgVer =
  H.Statement sql (arrayParam HE.text) (decodeSourceColumns cols) True
  -- query explanation at https://gist.github.com/steve-chavez/7ee0e6590cddafb532e5f00c46275569
  where
    subselectRegex :: Text
    -- "result" appears when the subselect is used inside "case when", see `authors_have_book_in_decade` fixture
    -- "resno"  appears in every other case
    -- when copying the query into pg make sure you omit one backslash from \\d+, it should be like `\d+` for the regex
    subselectRegex | pgVer < pgVersion100 = ":subselect {.*?:constraintDeps <>} :location \\d+} :res(no|ult)"
                   | otherwise = ":subselect {.*?:stmt_len 0} :location \\d+} :res(no|ult)"
    sql = [qc|
      with
      views as (
        select
          n.nspname   as view_schema,
          c.relname   as view_name,
          r.ev_action as view_definition
        from pg_class c
        join pg_namespace n on n.oid = c.relnamespace
        join pg_rewrite r on r.ev_class = c.oid
        where c.relkind in ('v', 'm') and n.nspname = ANY ($1)
      ),
      removed_subselects as(
        select
          view_schema, view_name,
          regexp_replace(view_definition, '{subselectRegex}', '', 'g') as x
        from views
      ),
      target_lists as(
        select
          view_schema, view_name,
          regexp_split_to_array(x, 'targetList') as x
        from removed_subselects
      ),
      last_target_list_wo_tail as(
        select
          view_schema, view_name,
          (regexp_split_to_array(x[array_upper(x, 1)], ':onConflict'))[1] as x
        from target_lists
      ),
      target_entries as(
        select
          view_schema, view_name,
          unnest(regexp_split_to_array(x, 'TARGETENTRY')) as entry
        from last_target_list_wo_tail
      ),
      results as(
        select
          view_schema, view_name,
          substring(entry from ':resname (.*?) :') as view_colum_name,
          substring(entry from ':resorigtbl (.*?) :') as resorigtbl,
          substring(entry from ':resorigcol (.*?) :') as resorigcol
        from target_entries
      )
      select
        sch.nspname as table_schema,
        tbl.relname as table_name,
        col.attname as table_column_name,
        res.view_schema,
        res.view_name,
        res.view_colum_name
      from results res
      join pg_class tbl on tbl.oid::text = res.resorigtbl
      join pg_attribute col on col.attrelid = tbl.oid and col.attnum::text = res.resorigcol
      join pg_namespace sch on sch.oid = tbl.relnamespace
      where resorigtbl <> '0'
      order by view_schema, view_name, view_colum_name; |]

getPgVersion :: H.Session PgVersion
getPgVersion = H.statement () $ H.Statement sql HE.noParams versionRow False
  where
    sql = "SELECT current_setting('server_version_num')::integer, current_setting('server_version')"
    versionRow = HD.singleRow $ PgVersion <$> column HD.int4 <*> column HD.text
