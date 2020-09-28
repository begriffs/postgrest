# Change Log

All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased

### Added

 - #1525, Allow http status override through response.status guc - @steve-chavez
 - #1512, Allow schema cache reloading with NOTIFY - @steve-chavez
 - #1119, Allow config file reloading with SIGUSR2 - @steve-chavez
 - #1558, Allow 'Bearer' with and without capitalization as authentication schema - @wolfgangwalther
 - #1559, No downtime when reloading the schema cache with SIGUSR1 - @steve-chavez
 - #1582, Allow overriding `Accept` and `Content-Type` on RPC with `pgrst.accept` - @steve-chavez, @Dansvidania, @wolfgangwalther

### Fixed

 - #1530, Fix how the PostgREST version is shown in the help text when the `.git` directory is not available - @monacoremo
 - #1094, Fix expired JWTs starting an empty transaction on the db - @steve-chavez
 - #1475, Fix location header for POST request with select= without PK (#1162) - @wolfgangwalther
 - #1582, Fix `application/octet-stream` appending `charset=utf-8` - @steve-chavez

### Changed

 - #1522, #1528, #1535, Docker images are now built from scratch based on a the static PostgREST executable (#1494) and with Nix instead of a `Dockerfile`. This reduces the compressed image size from over 30mb to about 4mb - @monacoremo
 - #1475, Location header for POST request is only included when PK is available on the table (#1461) - @wolfgangwalther
 - #1560, Volatile RPC called with GET now returns 405 Method not Allowed instead of 500 - @wolfgangwalther

### Deprecated

- #1582, Deprecate `raw-media-types` config. RPC setting `pgrst.accept` should be used instead - @steve-chavez

## [7.0.1] - 2020-05-18

### Fixed

- #1473, Fix overloaded computed columns on RPC - @wolfgangwalther
- #1471, Fix POST, PATCH, DELETE with ?select= and return=minimal and PATCH with empty body - @wolfgangwalther
- #1500, Fix missing `openapi-server-proxy-uri` config option - @steve-chavez
- #1508, Fix `Content-Profile` not working for POST RPC - @steve-chavez
- #1452, Fix PUT restriction for all columns - @steve-chavez

### Changed

- From this version onwards, the release page will only include a single Linux static executable that can be run on any Linux distribution.

## [7.0.0] - 2020-04-03

### Added

- #1417, `Accept: application/vnd.pgrst.object+json` behavior is now enforced for POST/PATCH/DELETE regardless of `Prefer: return=representation/minimal` - @dwagin
- #1415, Add support for user defined socket permission via `server-unix-socket-mode` config option - @Dansvidania
- #1383, Add support for HEAD request - @steve-chavez
- #1378, Add support for `Prefer: count=planned` and `Prefer: count=estimated` on GET /table - @steve-chavez, @LorenzHenk
- #1327, Add support for optional query parameter `on_conflict` to upsert with specified keys for POST - @ykst
- #1430, Allow specifying the foreign key constraint name(`/source?select=fk_constraint(*)`) to disambiguate an embedding - @steve-chavez
- #1168, Allow access to the `Authorization` header through the `request.header.authorization` GUC - @steve-chavez
- #1435, Add `request.method` and `request.path` GUCs - @steve-chavez
- #1088, Allow adding headers to GET/POST/PATCH/PUT/DELETE responses through the `response.headers` GUC - @steve-chavez
- #1427, Allow overriding provided headers(Location, Content-Type, etc) through the `response.headers` GUC - @steve-chavez
- #1450, Allow multiple schemas to be exposed in one instance. The schema to use can be selected through the headers `Accept-Profile` for GET/HEAD and `Content-Profile` for POST/PATCH/PUT/DELETE - @steve-chavez, @mahmoudkassem

### Fixed

- #1301, Fix self join resource embedding on PATCH - @herulume, @steve-chavez
- #1389, Fix many to many resource embedding on RPC/PATCH - @steve-chavez
- #1355, Allow PATCH/DELETE without `return=minimal` on tables with no select privileges - @steve-chavez
- #1361, Fix embedding a VIEW when its source foreign key is UNIQUE - @bwbroersma

### Changed

- #1385, bulk RPC call now should be done by specifying a `Prefer: params=multiple-objects` header - @steve-chavez
- #1401, resource embedding now outputs an error when multiple relationships between two tables are found - @steve-chavez
- #1423, default Unix Socket file mode from 755 to 660 - @dwagin
- #1430, Remove embedding with duck typed column names `GET /projects?select=client(*)`- @steve-chavez
  + You can rename the foreign key to `client` to make this request work in the new version: `alter table projects rename constraint projects_client_id_fkey to client`
- #1413, Change `server-proxy-uri` config option to `openapi-server-proxy-uri` - @steve-chavez

## [6.0.2] - 2019-08-22

### Fixed

- #1369, Change `raw-media-types` to accept a string of comma separated MIME types - @Dansvidania
- #1368, Fix long column descriptions being truncated at 63 characters in PostgreSQL 12 - @amedeedaboville
- #1348, Go back to converting plus "+" to space " " in querystrings by default - @steve-chavez

### Deprecated

- #1348, Deprecate `.` symbol for disambiguating resource embedding(added in #918). The url-safe '!' should be used instead. We refrained from using `+` as part of our syntax because it conflicts with some http clients and proxies.

## [6.0.1] - 2019-07-30

### Added

- #1349, Add user defined raw output media types via `raw-media-types` config option - @Dansvidania
- #1243, Add websearch_to_tsquery support - @herulume

### Fixed

- #1336, Error when testing on Chrome/Firefox: text/html requested but a single column was not selected - @Dansvidania
- #1334, Unable to compile v6.0.0 on windows - @steve-chavez

## [6.0.0] - 2019-06-21

### Added

- #1186, Add support for user defined unix socket via `server-unix-socket` config option - @Dansvidania
- #690, Add `?columns` query parameter for faster bulk inserts, also ignores unspecified json keys in a payload - @steve-chavez
- #1239, Add support for resource embedding on materialized views - @vitorbaptista
- #1264, Add support for bulk RPC call - @steve-chavez
- #1278, Add db-pool-timeout config option - @qu4tro
- #1285, Abort on wrong database password - @qu4tro
- #790, Allow override of OpenAPI spec through `root-spec` config option - @steve-chavez
- #1308, Accept `text/plain` and `text/html` for raw output - @steve-chavez


### Fixed

- #1223, Fix incorrect OpenAPI externalDocs url - @steve-chavez
- #1221, Fix embedding other resources when having a self join - @steve-chavez
- #1242, Fix embedding a view having a select in a where - @steve-chavez
- #1238, Fix PostgreSQL to OpenAPI type mappings for numeric and character types - @fpusch
- #1265, Fix query generated on bulk upsert with an empty array - @qu4tro
- #1273, Fix RPC ignoring unknown arguments by default - @steve-chavez
- #1257, Fix incorrect status when a PATCH request doesn't find rows to change - @qu4tro

### Changed

- #1288, Change server-host default of 127.0.0.1 to !4

### Deprecated

- #1288, Deprecate `.` symbol for disambiguating resource embedding(added in #918). '+' should be used instead. Though '+' is url safe, certain clients might need to encode it to '%2B'.

### Removed

- #1288, Removed support for schema reloading with SIGHUP, SIGUSR1 should be used instead - @steve-chavez

## [5.2.0] - 2018-12-12

### Added

- #1205, Add support for parsing JSON Web Key Sets - @russelldavies
- #1203, Add support for reading db-uri from a separate file - @zhoufeng1989
- #1200, Add db-extra-search-path config for adding schemas to the search_path, solves issues related to extensions created on the public schema - @steve-chavez
- #1219, Add ability to quote column names on filters - @steve-chavez

### Fixed

- #1182, Fix embedding on views with composite pks - @steve-chavez
- #1180, Fix embedding on views with subselects in pg10 - @steve-chavez
- #1197, Allow CORS for PUT - @bkylerussell
- #1181, Correctly qualify function argument of custom type in public schema - @steve-chavez
- #1008, Allow columns that contain spaces in filters - @steve-chavez

## [5.1.0] - 2018-08-31

### Added

- #1099, Add support for getting json/jsonb by array index - @steve-chavez
- #1145, Add materialized view columns to OpenAPI output - @steve-chavez
- #709, Allow embedding on views with subselects/CTE - @steve-chavez
- #1148, OpenAPI: add `required` section for the non-nullable columns - @laughedelic
- #1158, Add summary to OpenAPI doc for RPC functions - @mdr1384

### Fixed

- #1113, Fix UPSERT failing when having a camel case PK column - @steve-chavez
- #945, Fix slow start-up time on big schemas - @steve-chavez
- #1129, Fix view embedding when table is capitalized - @steve-chavez
- #1149, OpenAPI: Change `GET` response type to array - @laughedelic
- #1152, Fix RPC failing when having arguments with reserved or uppercase keywords - @mdr1384
- #905, Fix intermittent empty replies - @steve-chavez
- #1139, Fix JWTIssuedAtFuture failure for valid iat claim - @steve-chavez
- #1141, Fix app.settings resetting on pool timeout - @steve-chavez

### Changed

- #1099, Numbers in json path `?select=data->1->>key` now get treated as json array indexes instead of keys - @steve-chavez
- #1128, Allow finishing a json path with a single arrow `->`. Now a json can be obtained without resorting to casting, Previously: `/json_arr?select=data->>2::json`, now: `/json_arr?select=data->2` - @steve-chavez
- #724, Change server-host default of *4 to 127.0.0.1

### Deprecated

- #724, SIGHUP deprecated, SIGUSR1 should be used instead

## [0.5.0.0] - 2018-05-14

### Added

- The configuration (e.g. `postgrest.conf`) now accepts arbitrary settings that will be passed through as session-local database settings. This can be used to pass in secret keys directly as strings, or via OS environment variables. For instance: `app.settings.jwt_secret = "$(MYAPP_JWT_SECRET)"` will take `MYAPP_JWT_SECRET` from the environment and make it available to postgresql functions as `current_setting('app.settings.jwt_secret')`. Only `app.settings.*` values in the configuration file are treated in this way. - @canadaduane
- #256, Add support for bulk UPSERT with POST and single UPSERT with PUT - @steve-chavez
- #1078, Add ability to specify source column in embed - @steve-chavez
- #821, Allow embeds alias to be used in filters - @steve-chavez
- #906, Add jspath configurable `role-claim-key` - @steve-chavez
- #1061, Add foreign tables to OpenAPI output - @rhyamada

### Fixed

- #828, Fix computed column only working in public schema - @steve-chavez
- #925, Fix RPC high memory usage by using parametrized query and avoiding json encoding - @steve-chavez
- #987, Fix embedding with self-reference foreign key - @steve-chavez
- #1044, Fix view parent embedding when having many views - @steve-chavez
- #781, Fix accepting misspelled desc/asc ordering modificators - @onporat, @steve-chavez

### Changed

- #828, A `SET SCHEMA <db-schema>` is done on each request, this has the following implications:
  - Computed columns now only work if they belong to the db-schema
  - Stored procedures might require a `search_path` to work properly, for further details see https://postgrest.org/en/v5.0/api.html#explicit-qualification
- To use RPC now the `json_to_record/json_to_recordset` functions are needed, these are available starting from PostgreSQL 9.4 - @steve-chavez
- Overloaded functions now depend on the `dbStructure`, restart/sighup may be needed for their correct functioning - @steve-chavez
- #1098, Removed support for:
  + curly braces `{}` in embeds, i.e. `/clients?select=*,projects{*}` can no longer be used, from now on parens `()` should be used `/clients?select=*,projects(*)` - @steve-chavez
  + "in" operator without parens, i.e. `/clients?id=in.1,2,3` no longer supported, `/clients?id=in.(1,2,3)` should be used - @steve-chavez
  + "@@", "@>" and "<@" operators, from now on their mnemonic equivalents should be used "fts", "cs" and "cd" respectively - @steve-chavez

## [0.4.4.0] - 2018-01-08

### Added

- #887, #601, #1007, Allow specifying dictionary and plain/phrase tsquery in full text search - @steve-chavez
- #328, Allow doing GET on rpc - @steve-chavez
- #917, Add ability to map RAISE errorcode/message to http status - @steve-chavez
- #940, Add ability to map GUC to http response headers - @steve-chavez
- #1022, Include git sha in version report - @begriffs
- Faster queries using json_agg - @ruslantalpa

### Fixed

- #876, Read secret files as binary, discard final LF if any - @eric-brechemier
- #968, Treat blank proxy uri as missing - @begriffs
- #933, OpenAPI externals docs url to current version - @steve-chavez
- #962, OpenAPI don't err on nonexistent schema - @steve-chavez
- #954, make OpenAPI rpc output dependent on user privileges - @steve-chavez
- #955, Support configurable aud claim - @statik
- #996, Fix embedded column conflicts table name - @grotsev
- #974, Fix RPC error when function has single OUT param - @steve-chavez
- #1021, Reduce join size in allColumns for faster program start - @nextstopsun
- #411, Remove the need for pk in &select for parent embed - @steve-chavez
- #1016, Fix anonymous requests when configured with jwt-aud - @ruslantalpa

## [0.4.3.0] - 2017-09-06

### Added

- #567, Support more JWT signing algorithms, including RSA - @begriffs
- #889, Allow more than two conditions in a single and/or - @steve-chavez
- #883, Binary output support for RPC - @steve-chavez
- #885, Postgres COMMENTs on SCHEMA/TABLE/COLUMN are used for OpenAPI - @ldesgoui
- #907, Ability to embed using a specific relation when there are multiple between tables - @ruslantalpa
- #930, Split table comment on newline to get OpenAPI operation summary and description - @daurnimator
- #938, Support for range operators - @russelldavies

### Fixed

- #877, Base64 secret read from a file ending with a newline - @eric-brechemier
- #896, Boolean env var interpolation in config file - @begriffs
- #885, OpenAPI repetition reduced by using more definitions- @ldesgoui
- #924, Improve relations initialization time - @9too
- #927, Treat blank pre-request as missing - @begriffs

### Changed

- #938, Deprecate symbol operators with mnemonic names. - @russelldavies

## [0.4.2.0] - 2017-06-11

### Added

- #742, Add connection retrying on startup and SIGHUP - @steve-chavez
- #652, Add and/or params for complex boolean logic - @steve-chavez
- #808, Env var interpolation in config file (helps Docker) - @begriffs
- #878 - CSV output support for RPC - @begriffs

### Fixed

- #822, Treat blank string JWT secret as no secret - @begriffs

## [0.4.1.0] - 2017-04-25

### Added
- Allow requesting binary output on GET - @steve-chavez
- Accept clients requesting `Content-Type: application/json` from / - @feynmanliang
- #493, Updating with empty JSON object makes zero updates @koulakis
- Make HTTP headers and cookies available as GUCs #800 - @ruslantalpa
- #701, Ability to quote values on IN filters - @steve-chavez
- #641, Allow IN filter to have no values - @steve-chavez

### Fixed
- #827, Avoid Warp reaper, extend socket timeout to 1 hour - @majorcode
- #791, malformed nested JSON error - @diogob
- Resource embedding in views referencing tables in public schema - @fab1an
- #777, Empty body is allowed when calling a non-parameterized RPC - @koulakis
- #831, Fix proc resource embedding issue with search_path - @steve-chavez
- #547, Use read-only transaction for stable/immutable RPC - @begriffs

## [0.4.0.0] - 2017-01-19

### Added
- Allow test database to be on another host - @dsimunic
- `Prefer: params=single-object` to treat payload as single json argument in RPC - @dsimunic
- Ability to generate an OpenAPI spec - @mainx07, @hudayou, @ruslantalpa, @begriffs
- Ability to generate an OpenAPI spec behind a proxy - @hudayou
- Ability to set addresses to listen on - @hudayou
- Filtering, shaping and embedding with &select for the /rpc path - @ruslantalpa
- Output names of used-defined types (instead of 'USER-DEFINED') - @martingms
- Implement support for singular representation responses for POST/PATCH requests - @ehamberg
- Include RPC endpoints in OpenAPI output - @begriffs, @LogvinovLeon
- Custom request validation with `--pre-request` argument - @begriffs
- Ability to order by jsonb keys - @steve-chavez
- Ability to specify offset for a deeper level - @ruslantalpa
- Ability to use binary base64 encoded secrets - @TrevorBasinger

### Fixed
- Do not apply limit to parent items - @ruslantalpa
- Fix bug in relation detection when selecting parents two levels up by using the name of the FK - @ruslantalpa
- Customize content negotiation per route - @begriffs
- Allow using nulls order without explicit order direction - @steve-chavez
- Fatal error on postgres unsupported version, format supported version in error message - @steve-chavez
- Prevent database memory cosumption by prepared statements caches - @ruslantalpa
- Use specific columns in the RETURNING section - @ruslantalpa
- Fix columns alias for RETURNING - @steve-chavez

### Changed
- Replace `Prefer: plurality=singular` with `Accept: application/vnd.pgrst.object` - @begriffs
- Standardize arrays in responses for `Prefer: return=representation` - @begriffs
- Calling unknown RPC gives 404, not 400 - @begriffs
- Use HTTP 400 for raise\_exception - @begriffs
- Remove non-OpenAPI schema description - @begriffs
- Use comma rather than semicolon to separate Prefer header values - @begriffs
- Omit total query count by default - @begriffs
- No more reserved `jwt_claims` return type - @begriffs
- HTTP 401 rather than 400 for expired JWT - @begriffs
- Remove default JWT secret - @begriffs
- Use GUC request.jwt.claim.foo rather than postgrest.claims.foo - @begriffs
- Use config file rather than command line arguments - @begriffs

## [0.3.2.0] - 2016-06-10

### Added
- Reload database schema on SIGHUP - @begriffs
- Support "-" in column names - @ruslantalpa
- Support column/node renaming `alias:column` - @ruslantalpa
- Accept posts from HTML forms - @begriffs
- Ability to order embedded entities - @ruslantalpa
- Ability to paginate using &limit and &offset parameters - @ruslantalpa
- Ability to apply limits to embedded entities and enforce --max-rows on all levels - @ruslantalpa, @begriffs
- Add allow response header in OPTIONS - @begriffs

### Fixed
- Return 401 or 403 for access denied rather than 404 - @begriffs
- Omit Content-Type header for empty body - @begriffs
- Prevent role from being changed twice - @begriffs
- Use read-only transaction for read requests - @ruslantalpa
- Include entities from the same parent table using two different foreign keys - @ruslantalpa
- Ensure that Location header in 201 response is URL-encoded - @league
- Fix garbage collector CPU leak - @ruslantalpa et al.
- Return deleted items when return=representation header is sent - @ruslantalpa
- Use table default values for empty object inserts - @begriffs

## [0.3.1.1] - 2016-03-28

### Fixed
- Preserve unicode values in insert,update,rpc (regression) - @begriffs
- Prevent duplicate call to stored procs (regression) - @begriffs
- Allow SQL functions to generate registered JWT claims - @begriffs
- Terminate gracefully on SIGTERM (for use in Docker) - @recmo
- Relation detection fix for views that depend on multiple tables - @ruslantalpa
- Avoid count on plurality=singular and allow multiple Prefer values - @ruslantalpa

## [0.3.1.0] - 2016-02-28

### Fixed
- Prevent query error from infecting later connection - @begriffs, @ruslantalpa, @nikita-volkov, @jwiegley

### Added
- Applies range headers to RPC calls - @diogob

## [0.3.0.4] - 2016-02-12

### Fixed
- Improved usage screen - @begriffs
- Reject non-POSTs to rpc endpoints - @begriffs
- Throw an error for OPTIONS on nonexistent tables - @calebmer
- Remove deadlock on simultaneous contentious updates - @ruslantalpa, @begriffs

## [0.3.0.3] - 2016-01-08

### Fixed
- Fix bug in many-many relation detection - @ruslantalpa
- Inconsistent escaping of table names in read queries - @calebmer

## [0.3.0.2] - 2015-12-16

### Fixed
- Miscalculation of time used for expiring tokens - @calebmer
- Remove bcrypt dependency to fix Windows build - @begriffs
- Detect relations event when authenticator does not have rights to intermediate tables - @ruslantalpa
- Ensure db connections released on sigint - @begriffs
- Fix #396 include records with missing parents - @ruslantalpa
- `pgFmtIdent` always quotes #388 - @calebmer
- Default schema, changed from `"1"` to `public` - @calebmer
- #414 revert to separate count query - @ruslantalpa
- Fix #399, allow inserting in tables with no select privileges using "Prefer: representation=minimal" - @ruslantalpa

### Added
- Allow order by computed columns - @diogob
- Set max rows in response with --max-rows - @begriffs
- Selection by column name (can detect if `_id` is not included) - @calebmer

## [0.3.0.1] - 2015-11-27

### Fixed
- Filter columns on embedded parent items - @ruslantalpa

## [0.3.0.0] - 2015-11-24

### Fixed
- Use reasonable amount of memory during bulk inserts - @begriffs

### Added
- Ensure JWT expires - @calebmer
- Postgres connection string argument - @calebmer
- Encode JWT for procs that return type `jwt_claims` - @diogob
- Full text operators `@>`,`<@` - @ruslantalpa
- Shaping of the response body (filter columns, embed relations) with &select parameter for POST/PATCH - @ruslantalpa
- Detect relationships between public views and private tables - @calebmer
- `Prefer: plurality=singular` for selecting single objects - @calebmer

### Removed
- API versioning feature - @calebmer
- `--db-x` command line arguments - @calebmer
- Secure flag - @calebmer
- PUT request handling - @ruslantalpa

### Changed
- Embed foreign keys with {} rather than () - @begriffs
- Remove version number from binary filename in release - @begriffs

## [0.2.12.1] - 2015-11-12

### Fixed
- Correct order for -> and ->> in a json path - @ruslantalpa
- Return empty array instead of 500 when a set returning function returns an empty result set - @diogob

## [0.2.12.0] - 2015-10-25

### Added
- Embed associations, e.g. `/film?select=*,director(*)` - @ruslantalpa
- Filter columns, e.g. `?select=col1,col2` - @ruslantalpa
- Does not execute the count total if header "Prefer: count=none" - @diogob

### Fixed
- Tolerate a missing role in user creation - @calebmer
- Avoid unnecessary text re-encoding - @ruslantalpa

## [0.2.11.1] - 2015-09-01

### Fixed
- Accepts `*/*` in Accept header - @diogob

## [0.2.11.0] - 2015-08-28
### Added
- Negate any filter in a uniform way, e.g. `?col=not.eq=foo` - @diogob
- Call stored procedures
- Filter NOT IN values, e.g. `?col=notin.1,2,3` - @rall
- CSV responses to GET requests with `Accept: text/csv` - @diogob
- Debian init scripts - @mkhon
- Allow filters by computed columns - @diogob

### Fixed
- Reset user role on error
- Compatible with Stack
- Add materialized views to results in GET / - @diogob
- Indicate insertable=true for views that are insertable through triggers - @diogob
- Builds under GHC 7.10
- Allow the use of columns named "count" in relations queried - @diogob

## [0.2.10.0] - 2015-06-03
### Added
- Full text search, eg `/foo?text_vector=@@.bar`
- Include auth id as well as db role to views (for row-level security)

## [0.2.9.1] - 2015-05-20
### Fixed
- Put -Werror behind a cabal flag (for CI) so Hackage accepts package

## [0.2.9.0] - 2015-05-20
### Added
- Return range headers in PATCH
- Return PATCHed resources if header "Prefer: return=representation"
- Allow nested objects and arrays in JSON post for jsonb columns
- JSON Web Tokens - [Federico Rampazzo](https://github.com/framp)
- Expose PostgREST as a Haskell package

### Fixed
- Return 404 if no records updated by PATCH

## [0.2.8.0] - 2015-04-17
### Added
- Option to specify nulls first or last, eg `/people?order=age.desc.nullsfirst`
- Filter nulls, `?col=is.null` and `?col=isnot.null`
- Filter within jsonb, `?col->a->>b=eq.c`
- Accept CSV in post body for bulk inserts

### Fixed
- Allow NULL values in posts
- Show full command line usage on param errors

## [0.2.7.0] - 2015-03-03
### Added
- Server response logging
- Filter IN values, e.g. `?col=in.1,2,3`
- Return POSTed resource if header "Prefer: return=representation"
- Allow override of default (v1) schema

## [0.2.6.0] - 2015-02-18
### Added
- A changelog
- Filter by substring match, e.g. `?col=like.*hello*` (or ilike for
  case insensitivity).
- Access-Control-Expose-Headers for CORS

### Fixed
- Make filter position match docs, e.g. `?order=col.asc` rather
  than `?order=asc.col`.
