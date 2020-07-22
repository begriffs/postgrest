-- Privileges for anonymous
GRANT USAGE ON SCHEMA
      postgrest
    , test
    , jwt
    , public
    , "تست"
    , extensions
    , v1
    , v2
TO postgrest_test_anonymous;

-- Schema test objects
SET search_path = test, "تست", pg_catalog;

GRANT ALL ON TABLE
    items
    , items2
    , "articleStars"
    , articles
    , auto_incrementing_pk
    , clients
    , comments
    , complex_items
    , compound_pk
    , empty_table
    , has_count_column
    , has_fk
    , insertable_view_with_join
    , json
    , materialized_view
    , menagerie
    , no_pk
    , nullable_integer
    , projects
    , projects_view
    , projects_view_alt
    , simple_pk
    , tasks
    , filtered_tasks
    , tsearch
    , users
    , users_projects
    , users_tasks
    , "Escap3e;"
    , "ghostBusters"
    , "withUnique"
    , "clashing_column"
    , "موارد"
    , addresses
    , orders
    , public.public_consumers
    , public.public_orders
    , consumers_view
    , orders_view
    , images
    , images_base64
    , w_or_wo_comma_names
    , items_with_different_col_types
    , entities
    , child_entities
    , grandchild_entities
    , ranges
    , being
    , descendant
    , being_part
    , part
    , leak
    , perf_articles
    , employees
    , tiobe_pls
    , single_unique
    , compound_unique
    , only_pk
    , family_tree
    , managers
    , organizations
    , authors
    , books
    , forties_books
    , fifties_books
    , sixties_books
    , person
    , message
    , person_detail
    , space
    , zone
    , projects_dump
    , "UnitTest"
    , json_arr
    , jsonb_test
    , authors_books_number
    , authors_have_book_in_decade
    , forties_and_fifties_books
    , odd_years_publications
    , foos
    , bars
    , materialized_projects
    , contract
    , player_view
    , contract_view
    , ltree_sample
    , isn_sample
    , projects_count_grouped_by
    , "Server Today"
    , pgrst_reserved_chars
    , authors_w_entities
    , openapi_types
    , getallprojects_view
    , get_projects_above_view
    , web_content
    , pages
    , referrals
    , big_projects
    , sites
    , jobs
    , main_jobs
    , whatev_projects
    , whatev_sites
    , whatev_jobs
    , agents
    , departments
    , schedules
    , activities
    , unit_workdays
    , stuff
    , loc_test
    , v1.parents
    , v2.parents
    , v2.another_table
    , v1.children
    , v2.children
    , shops
TO postgrest_test_anonymous;

GRANT INSERT ON TABLE insertonly TO postgrest_test_anonymous;

GRANT USAGE ON SEQUENCE
      auto_incrementing_pk_id_seq
    , items_id_seq
    , callcounter_count
    , leak_id_seq
    , v1.children_id_seq
    , v2.children_id_seq
TO postgrest_test_anonymous;

-- Privileges for non anonymous users
GRANT USAGE ON SCHEMA test TO postgrest_test_author;
GRANT ALL ON TABLE authors_only TO postgrest_test_author;

GRANT SELECT (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;
GRANT INSERT (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;
GRANT UPDATE (article_id, user_id) ON TABLE limited_article_stars TO postgrest_test_anonymous;

GRANT SELECT(id, email) ON TABLE app_users TO postgrest_test_anonymous;
GRANT INSERT, UPDATE    ON TABLE app_users TO postgrest_test_anonymous;
GRANT DELETE            ON TABLE app_users TO postgrest_test_anonymous;

REVOKE EXECUTE ON FUNCTION privileged_hello(text) FROM PUBLIC; -- All functions are available to every role(PUBLIC) by default
GRANT EXECUTE ON FUNCTION privileged_hello(text) TO postgrest_test_author;

GRANT USAGE ON SCHEMA test TO postgrest_test_default_role;
