--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5beta1
-- Dumped by pg_dump version 9.5beta1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = postgrest, pg_catalog;

--
-- Data for Name: auth; Type: TABLE DATA; Schema: postgrest; Owner: -
--

TRUNCATE TABLE auth CASCADE;
INSERT INTO auth VALUES ('jdoe', 'postgrest_test_author', '1234                                                        ');


SET search_path = private, pg_catalog;

--
-- Data for Name: articles; Type: TABLE DATA; Schema: private; Owner: -
--

TRUNCATE TABLE articles CASCADE;
INSERT INTO articles VALUES (1, 'No… It''s a thing; it''s like a plan, but with more greatness.', 'diogo');
INSERT INTO articles VALUES (2, 'Stop talking, brain thinking. Hush.', 'diogo');
INSERT INTO articles VALUES (3, 'It''s a fez. I wear a fez now. Fezes are cool.', 'diogo');


SET search_path = test, pg_catalog;

--
-- Data for Name: users; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE users CASCADE;
INSERT INTO users VALUES (1, 'Angela Martin');
INSERT INTO users VALUES (2, 'Michael Scott');
INSERT INTO users VALUES (3, 'Dwight Schrute');


SET search_path = private, pg_catalog;

--
-- Data for Name: article_stars; Type: TABLE DATA; Schema: private; Owner: -
--

TRUNCATE TABLE article_stars CASCADE;
INSERT INTO article_stars VALUES (1, 1, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (2, 3, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (3, 2, '2015-12-08 04:22:57.472738');
INSERT INTO article_stars VALUES (1, 3, '2015-12-08 04:22:57.472738');


SET search_path = test, pg_catalog;

--
-- Data for Name: authors_only; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE authors_only CASCADE;


--
-- Data for Name: auto_incrementing_pk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE auto_incrementing_pk CASCADE;


--
-- Name: auto_incrementing_pk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('auto_incrementing_pk_id_seq', 1, true);


--
-- Data for Name: clients; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE clients CASCADE;
INSERT INTO clients VALUES (1, 'Microsoft');
INSERT INTO clients VALUES (2, 'Apple');


--
-- Data for Name: projects; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE projects CASCADE;
INSERT INTO projects VALUES (1, 'Windows 7', 1);
INSERT INTO projects VALUES (2, 'Windows 10', 1);
INSERT INTO projects VALUES (3, 'IOS', 2);
INSERT INTO projects VALUES (4, 'OSX', 2);
INSERT INTO projects VALUES (5, 'Orphan', NULL);


--
-- Data for Name: tasks; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE tasks CASCADE;
INSERT INTO tasks VALUES (1, 'Design w7', 1);
INSERT INTO tasks VALUES (2, 'Code w7', 1);
INSERT INTO tasks VALUES (3, 'Design w10', 2);
INSERT INTO tasks VALUES (4, 'Code w10', 2);
INSERT INTO tasks VALUES (5, 'Design IOS', 3);
INSERT INTO tasks VALUES (6, 'Code IOS', 3);
INSERT INTO tasks VALUES (7, 'Design OSX', 4);
INSERT INTO tasks VALUES (8, 'Code OSX', 4);


--
-- Data for Name: users_tasks; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE users_tasks CASCADE;
INSERT INTO users_tasks VALUES (1, 1);
INSERT INTO users_tasks VALUES (1, 2);
INSERT INTO users_tasks VALUES (1, 3);
INSERT INTO users_tasks VALUES (1, 4);
INSERT INTO users_tasks VALUES (2, 5);
INSERT INTO users_tasks VALUES (2, 6);
INSERT INTO users_tasks VALUES (2, 7);
INSERT INTO users_tasks VALUES (3, 1);
INSERT INTO users_tasks VALUES (3, 5);


--
-- Data for Name: comments; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE comments CASCADE;
INSERT INTO comments VALUES (1, 1, 2, 6, 'Needs to be delivered ASAP');


--
-- Data for Name: complex_items; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE complex_items CASCADE;
INSERT INTO complex_items VALUES (1, 'One', '{"foo":{"int":1,"bar":"baz"}}', '{1}');
INSERT INTO complex_items VALUES (2, 'Two', '{"foo":{"int":1,"bar":"baz"}}', '{1,2}');
INSERT INTO complex_items VALUES (3, 'Three', '{"foo":{"int":1,"bar":"baz"}}', '{1,2,3}');


--
-- Data for Name: compound_pk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE compound_pk CASCADE;


--
-- Data for Name: simple_pk; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE simple_pk CASCADE;
INSERT INTO simple_pk VALUES ('xyyx', 'u');
INSERT INTO simple_pk VALUES ('xYYx', 'v');

--
-- Data for Name: has_fk; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE has_fk CASCADE;


--
-- Name: has_fk_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('has_fk_id_seq', 1, false);


--
-- Data for Name: items; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE items CASCADE;
INSERT INTO items VALUES (1);
INSERT INTO items VALUES (2);
INSERT INTO items VALUES (3);
INSERT INTO items VALUES (4);
INSERT INTO items VALUES (5);
INSERT INTO items VALUES (6);
INSERT INTO items VALUES (7);
INSERT INTO items VALUES (8);
INSERT INTO items VALUES (9);
INSERT INTO items VALUES (10);
INSERT INTO items VALUES (11);
INSERT INTO items VALUES (12);
INSERT INTO items VALUES (13);
INSERT INTO items VALUES (14);
INSERT INTO items VALUES (15);


--
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('items_id_seq', 15, true);


--
-- Data for Name: items2; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE items2 CASCADE;
INSERT INTO items2 VALUES (1);
INSERT INTO items2 VALUES (2);
INSERT INTO items2 VALUES (3);
INSERT INTO items2 VALUES (4);
INSERT INTO items2 VALUES (5);
INSERT INTO items2 VALUES (6);
INSERT INTO items2 VALUES (7);
INSERT INTO items2 VALUES (8);
INSERT INTO items2 VALUES (9);
INSERT INTO items2 VALUES (10);
INSERT INTO items2 VALUES (11);
INSERT INTO items2 VALUES (12);
INSERT INTO items2 VALUES (13);
INSERT INTO items2 VALUES (14);
INSERT INTO items2 VALUES (15);


--
-- Name: items_id_seq; Type: SEQUENCE SET; Schema: test; Owner: -
--

SELECT pg_catalog.setval('items2_id_seq', 15, true);


--
-- Data for Name: json; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE json CASCADE;
INSERT INTO json VALUES ('{"foo":{"bar":"baz"},"id":1}');
INSERT INTO json VALUES ('{"id":3}');
INSERT INTO json VALUES ('{"id":0}');


--
-- Data for Name: menagerie; Type: TABLE DATA; Schema: test; Owner: -
--
TRUNCATE TABLE menagerie CASCADE;


--
-- Data for Name: no_pk; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE no_pk CASCADE;
INSERT INTO no_pk VALUES (NULL, NULL);
INSERT INTO no_pk VALUES ('1', '0');
INSERT INTO no_pk VALUES ('2', '0');


--
-- Data for Name: nullable_integer; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE nullable_integer CASCADE;
INSERT INTO nullable_integer VALUES (NULL);


--
-- Data for Name: tsearch; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE tsearch CASCADE;
INSERT INTO tsearch VALUES (to_tsvector('It''s kind of fun to do the impossible'));
INSERT INTO tsearch VALUES (to_tsvector('But also fun to do what is possible'));
INSERT INTO tsearch VALUES (to_tsvector('Fat cats ate rats'));
INSERT INTO tsearch VALUES (to_tsvector('french', 'C''est un peu amusant de faire l''impossible'));
INSERT INTO tsearch VALUES (to_tsvector('german', 'Es ist eine Art Spaß, das Unmögliche zu machen'));

--
-- Data for Name: users_projects; Type: TABLE DATA; Schema: test; Owner: -
--

TRUNCATE TABLE users_projects CASCADE;
INSERT INTO users_projects VALUES (1, 1);
INSERT INTO users_projects VALUES (1, 2);
INSERT INTO users_projects VALUES (2, 3);
INSERT INTO users_projects VALUES (2, 4);
INSERT INTO users_projects VALUES (3, 1);
INSERT INTO users_projects VALUES (3, 3);

TRUNCATE TABLE "Escap3e;" CASCADE;
INSERT INTO "Escap3e;" VALUES (1), (2), (3), (4), (5);

TRUNCATE TABLE "ghostBusters" CASCADE;
INSERT INTO "ghostBusters" VALUES (1), (3), (5);

TRUNCATE TABLE "withUnique" CASCADE;
INSERT INTO "withUnique" VALUES ('nodup', 'blah');



TRUNCATE TABLE addresses CASCADE;
INSERT INTO addresses VALUES (1, 'address 1');
INSERT INTO addresses VALUES (2, 'address 2');
INSERT INTO addresses VALUES (3, 'address 3');
INSERT INTO addresses VALUES (4, 'address 4');

TRUNCATE TABLE orders CASCADE;
INSERT INTO orders VALUES (1, 'order 1', 1, 2);
INSERT INTO orders VALUES (2, 'order 2', 3, 4);

TRUNCATE TABLE images CASCADE;
INSERT INTO images(name, img) VALUES ('A.png', decode('iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEUAAAD/AAAb/40iAAAAP0lEQVQI12NgwAbYG2AE/wEYwQMiZB4ACQkQYZEAIgqAhAGIKLCAEQ8kgMT/P1CCEUwc4IMSzA3sUIIdCHECAGSQEkeOTUyCAAAAAElFTkSuQmCC', 'base64'));
INSERT INTO images(name, img) VALUES ('B.png', decode('iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeAQMAAAAB/jzhAAAABlBMVEX///8AAP94wDzzAAAAL0lEQVQIW2NgwAb+HwARH0DEDyDxwAZEyGAhLODqHmBRzAcn5GAS///A1IF14AAA5/Adbiiz/0gAAAAASUVORK5CYII=', 'base64'));

TRUNCATE TABLE w_or_wo_comma_names CASCADE;
INSERT INTO w_or_wo_comma_names VALUES ('Hebdon, John');
INSERT INTO w_or_wo_comma_names VALUES ('Williams, Mary');
INSERT INTO w_or_wo_comma_names VALUES ('Smith, Joseph');
INSERT INTO w_or_wo_comma_names VALUES ('David White');
INSERT INTO w_or_wo_comma_names VALUES ('Larry Thompson');

TRUNCATE TABLE items_with_different_col_types CASCADE;
INSERT INTO items_with_different_col_types VALUES (1, null, null, null, null, null, null, null);

TRUNCATE TABLE entities CASCADE;
INSERT INTO entities VALUES (1, 'entity 1', '{1}', '''bar'':2 ''foo'':1');
INSERT INTO entities VALUES (2, 'entity 2', '{1,2}', '''baz'':1 ''qux'':2');
INSERT INTO entities VALUES (3, 'entity 3', '{1,2,3}', null);
INSERT INTO entities VALUES (4, null, null, null);

TRUNCATE TABLE child_entities CASCADE;
INSERT INTO child_entities VALUES (1, 'child entity 1', 1);
INSERT INTO child_entities VALUES (2, 'child entity 2', 1);
INSERT INTO child_entities VALUES (3, 'child entity 3', 2);

TRUNCATE TABLE grandchild_entities CASCADE;
INSERT INTO grandchild_entities VALUES (1, 'grandchild entity 1', 1, null, null, null);
INSERT INTO grandchild_entities VALUES (2, 'grandchild entity 2', 1, null, null, null);
INSERT INTO grandchild_entities VALUES (3, 'grandchild entity 3', 2, null, null, null);
INSERT INTO grandchild_entities VALUES (4, '(grandchild,entity,4)', 2, null, null, '{"a": {"b":"foo"}}');
INSERT INTO grandchild_entities VALUES (5, '(grandchild,entity,5)', 2, null, null, '{"b":"bar"}');

TRUNCATE TABLE ranges CASCADE;
INSERT INTO ranges VALUES (1, '[1,3]');
INSERT INTO ranges VALUES (2, '[3,6]');
INSERT INTO ranges VALUES (3, '[6,9]');
INSERT INTO ranges VALUES (4, '[9,12]');

TRUNCATE TABLE being CASCADE;
INSERT INTO being VALUES (1), (2), (3), (4);

TRUNCATE TABLE descendant CASCADE;
INSERT INTO descendant VALUES (1,1), (2,1), (3,1), (4,2);

TRUNCATE TABLE part CASCADE;
INSERT INTO part VALUES (1), (2), (3), (4);

TRUNCATE TABLE being_part CASCADE;
INSERT INTO being_part VALUES (1,1), (2,1), (3,2), (4,3);

TRUNCATE TABLE employees CASCADE;
INSERT INTO employees VALUES
  ('Frances M.', 'Roe', '24000', 'One-Up Realty', 'Author'),
  ('Daniel B.', 'Lyon', '36000', 'Dubrow''s Cafeteria', 'Packer'),
  ('Edwin S.', 'Smith', '48000', 'Pro Garden Management', 'Marine biologist');

TRUNCATE TABLE tiobe_pls CASCADE;
INSERT INTO tiobe_pls VALUES ('Java', 1), ('C', 2), ('Python', 4);

TRUNCATE TABLE single_unique CASCADE;
INSERT INTO single_unique (unique_key, value) VALUES (1, 'A');

TRUNCATE TABLE compound_unique CASCADE;
INSERT INTO compound_unique (key1, key2, value) VALUES (1, 1, 'A');

TRUNCATE TABLE only_pk CASCADE;
INSERT INTO only_pk VALUES (1), (2);

TRUNCATE TABLE family_tree CASCADE;
INSERT INTO family_tree VALUES ('1', 'Parental Unit', NULL);
INSERT INTO family_tree VALUES ('2', 'Kid One', '1');
INSERT INTO family_tree VALUES ('3', 'Kid Two', '1');
INSERT INTO family_tree VALUES ('4', 'Grandkid One', '2');
INSERT INTO family_tree VALUES ('5', 'Grandkid Two', '3');

TRUNCATE TABLE managers CASCADE;
INSERT INTO managers VALUES (1, 'Referee Manager');
INSERT INTO managers VALUES (2, 'Auditor Manager');
INSERT INTO managers VALUES (3, 'Acme Manager');
INSERT INTO managers VALUES (4, 'Umbrella Manager');
INSERT INTO managers VALUES (5, 'Cyberdyne Manager');
INSERT INTO managers VALUES (6, 'Oscorp Manager');

TRUNCATE TABLE organizations CASCADE;
INSERT INTO organizations VALUES (1, 'Referee Org', null, null, 1);
INSERT INTO organizations VALUES (2, 'Auditor Org', null, null, 2);
INSERT INTO organizations VALUES (3, 'Acme', 1, 2, 3);
INSERT INTO organizations VALUES (4, 'Umbrella', 1, 2, 4);
INSERT INTO organizations VALUES (5, 'Cyberdyne', 3, 4, 5);
INSERT INTO organizations VALUES (6, 'Oscorp', 3, 4, 6);

SET search_path = private, pg_catalog;

TRUNCATE TABLE authors CASCADE;
INSERT INTO authors VALUES (1, 'George Orwell');
INSERT INTO authors VALUES (2, 'Anne Frank');
INSERT INTO authors VALUES (3, 'Antoine de Saint-Exupéry');
INSERT INTO authors VALUES (4, 'J.D. Salinger');
INSERT INTO authors VALUES (5, 'Ray Bradbury');
INSERT INTO authors VALUES (6, 'William Golding');
INSERT INTO authors VALUES (7, 'Harper Lee');
INSERT INTO authors VALUES (8, 'Kurt Vonnegut');
INSERT INTO authors VALUES (9, 'Ken Kesey');

TRUNCATE TABLE publishers CASCADE;
INSERT INTO publishers VALUES (1, 'Secker & Warburg');
INSERT INTO publishers VALUES (2, 'Contact Publishing');
INSERT INTO publishers VALUES (3, 'Reynal & Hitchcock');
INSERT INTO publishers VALUES (4, 'Little, Brown and Company');
INSERT INTO publishers VALUES (5, 'Ballantine Books');
INSERT INTO publishers VALUES (6, 'Faber and Faber');
INSERT INTO publishers VALUES (7, 'J. B. Lippincott & Co.');
INSERT INTO publishers VALUES (8, 'Delacorte');
INSERT INTO publishers VALUES (9, 'Viking Press & Signet Books');

TRUNCATE TABLE books CASCADE;
INSERT INTO books VALUES (1, '1984', 1949, 1, 1);
INSERT INTO books VALUES (2, 'The Diary of a Young Girl', 1947, 2, 2);
INSERT INTO books VALUES (3, 'The Little Prince', 1947, 3, 3);
INSERT INTO books VALUES (4, 'The Catcher in the Rye', 1951, 4, 4);
INSERT INTO books VALUES (5, 'Farenheit 451', 1953, 5, 5);
INSERT INTO books VALUES (6, 'Lord of the Flies', 1954, 6, 6);
INSERT INTO books VALUES (7, 'To Kill a Mockingbird', 1960, 7, 7);
INSERT INTO books VALUES (8, 'Slaughterhouse-Five', 1969, 8, 8);
INSERT INTO books VALUES (9, 'One Flew Over the Cuckoo''s Nest', 1962, 9, 9);

SET search_path = test, pg_catalog;

TRUNCATE TABLE person CASCADE;

INSERT INTO person VALUES (1, 'John');
INSERT INTO person VALUES (2, 'Jane');
INSERT INTO person VALUES (3, 'Jake');
INSERT INTO person VALUES (4, 'Julie');

TRUNCATE TABLE message CASCADE;
INSERT INTO message VALUES (1, 'Hello Jane', 1, 2);
INSERT INTO message VALUES (2, 'Hi John', 2, 1);
INSERT INTO message VALUES (3, 'How are you doing?', 1, 2);
INSERT INTO message VALUES (4, 'Hey Julie', 3, 4);
INSERT INTO message VALUES (5, 'What''s up Jake', 4, 3);

TRUNCATE TABLE space CASCADE;
INSERT INTO space VALUES (1, 'space 1');

TRUNCATE TABLE zone CASCADE;
INSERT INTO zone VALUES (1, 'zone 1', 2, 1);
INSERT INTO zone VALUES (2, 'zone 2', 2, 1);
INSERT INTO zone VALUES (3, 'store 3', 3, 1);
INSERT INTO zone VALUES (4, 'store 4', 3, 1);

-- for foreign table projects_dump
copy (select id, name, client_id from projects) to '/tmp/projects_dump.csv' with csv;

TRUNCATE TABLE "UnitTest" CASCADE;
INSERT INTO "UnitTest" VALUES (1, 'unit test 1');

TRUNCATE TABLE json_arr CASCADE;
INSERT INTO json_arr VALUES (1, '[1, 2, 3]');
INSERT INTO json_arr VALUES (2, '[4, 5, 6]');
INSERT INTO json_arr VALUES (3, '[[9, 8, 7], [11, 12, 13]]');
INSERT INTO json_arr VALUES (4, '[[[5, 6], 7, 8]]');
INSERT INTO json_arr VALUES (5, '[{"a": "A"}, {"b": "B"}]');
INSERT INTO json_arr VALUES (6, '[{"a": [1,2,3]}, {"b": [4,5]}]');
INSERT INTO json_arr VALUES (7, '{"c": [1,2,3], "d": [4,5]}');
INSERT INTO json_arr VALUES (8, '{"c": [{"d": [4,5,6,7,8]}]}');
INSERT INTO json_arr VALUES (9, '[{"0xy1": [1,{"23-xy-45": [2, {"xy-6": [3]}]}]}]');

TRUNCATE TABLE jsonb_test CASCADE;
INSERT INTO jsonb_test VALUES (1, '{ "a": {"b": 2} }');
INSERT INTO jsonb_test VALUES (2, '{ "c": [1,2,3] }');
INSERT INTO jsonb_test VALUES (3, '[{ "d": "test" }]');
INSERT INTO jsonb_test VALUES (4, '{ "e": 1 }');

TRUNCATE TABLE private.player CASCADE;
INSERT into private.player
SELECT
  generate_series,
  'first_name_' || generate_series,
  'last_name_' || generate_series,
  '2018-10-11'
FROM generate_series(1, 12);

TRUNCATE TABLE contract CASCADE;
insert into contract
select
  'tournament_' || generate_series,
  tsrange(now()::timestamp, null),
  10*generate_series,
  generate_series,
  'first_name_' || generate_series,
  'last_name_' || generate_series,
  '2018-10-11'
from generate_series(1, 6);

TRUNCATE TABLE ltree_sample CASCADE;
INSERT INTO ltree_sample VALUES ('Top');
INSERT INTO ltree_sample VALUES ('Top.Science');
INSERT INTO ltree_sample VALUES ('Top.Science.Astronomy');
INSERT INTO ltree_sample VALUES ('Top.Science.Astronomy.Astrophysics');
INSERT INTO ltree_sample VALUES ('Top.Science.Astronomy.Cosmology');

TRUNCATE TABLE isn_sample CASCADE;
INSERT INTO isn_sample VALUES ('978-0-393-04002-9', 'Mathematics: From the Birth of Numbers');

TRUNCATE TABLE "Server Today" CASCADE;
COPY "Server Today" ("cHostname", "Just A Server Model") FROM STDIN CSV DELIMITER '|';
argnim1    | IBM,9113-550 (P5-550)
argnim2    | IBM,9113-550 (P5-550)
daaa2nim71 | IBM,9131-52A (P5-52A)
daah3nim71 | IBM,8406-71Y (P7-PS701)
hbnim1     | IBM,9133-55A (P5-55A)
\.

TRUNCATE TABLE pgrst_reserved_chars CASCADE;
COPY pgrst_reserved_chars ("*id*", ":arr->ow::cast", "(inside,parens)", "a.dotted.column", "  col  w  space  ") FROM STDIN CSV DELIMITER '|';
1 | arrow-1 | parens-1 | dotted-1 | space-1
2 | arrow-2 | parens-2 | dotted-2 | space-2
3 | arrow-3 | parens-3 | dotted-3 | space-3
\.

TRUNCATE TABLE web_content CASCADE;
INSERT INTO web_content VALUES (5, 'wat', null);
INSERT INTO web_content VALUES (0, 'tardis', 5);
INSERT INTO web_content VALUES (1, 'fezz', 0);
INSERT INTO web_content VALUES (2, 'foo', 0);
INSERT INTO web_content VALUES (3, 'bar', 0);
INSERT INTO web_content VALUES (4, 'wut', 1);

TRUNCATE TABLE app_users CASCADE;
INSERT INTO app_users (id, email, "password") VALUES (1, 'test@123.com','pass');
INSERT INTO app_users (id, email, "password") VALUES (2, 'abc@123.com','pass');
INSERT INTO app_users (id, email, "password") VALUES (3, 'def@123.com','pass');

TRUNCATE TABLE private.pages CASCADE;
INSERT INTO private.pages VALUES (1, 'http://postgrest.org/en/v6.0/api.html');
INSERT INTO private.pages VALUES (2, 'http://postgrest.org/en/v6.0/admin.html');

TRUNCATE TABLE private.referrals CASCADE;
INSERT INTO private.referrals VALUES ('github.com', 1);
INSERT INTO private.referrals VALUES ('hub.docker.com', 2);

TRUNCATE TABLE big_projects CASCADE;
INSERT INTO big_projects (big_project_id, name)
VALUES (1, 'big project 1'),
       (2, 'big project 2');

TRUNCATE TABLE sites CASCADE;
INSERT INTO sites (site_id, name, main_project_id)
VALUES (1, 'site 1', 1),
       (2, 'site 2', null),
       (3, 'site 3', 2),
       (4, 'site 4', null);

TRUNCATE TABLE jobs CASCADE;
INSERT INTO jobs (job_id, name, site_id, big_project_id)
VALUES ('bc5d5362-b881-438f-b9f5-7417e08704ed', 'job 1-1', 1, 1),
       ('3bd52697-033b-4edd-8a28-46a9c04b7c1e', 'job 2-1', 2, 1),
       ('e6e67e4e-19b1-11e9-ab14-d663bd873d93', 'job 2-2', 2, 2);

TRUNCATE TABLE departments CASCADE;
TRUNCATE TABLE agents CASCADE;
INSERT INTO agents (id, name)
VALUES (1, 'agent 1'),
       (2, 'agent 2'),
       (3, 'agent 3'),
       (4, 'agent 4');

INSERT INTO departments (id, name, head_id)
VALUES (1, 'dep 1', 1),
       (2, 'dep 3', 3);

UPDATE agents SET department_id = 1 WHERE id in (1, 2);
UPDATE agents SET department_id = 2 WHERE id in (3, 4);

TRUNCATE TABLE schedules CASCADE;
INSERT INTO schedules VALUES(1, 'morning', '06:00:00', '11:59:00');
INSERT INTO schedules VALUES(2, 'afternoon', '12:00:00', '17:59:00');
INSERT INTO schedules VALUES(3, 'night', '18:00:00', '23:59:00');
INSERT INTO schedules VALUES(4, 'early morning', '00:00:00', '05:59:00');

TRUNCATE TABLE activities CASCADE;
INSERT INTO activities(id, schedule_id, car_id)    VALUES(1, 1, 'CAR-349');
INSERT INTO activities(id, schedule_id, camera_id) VALUES(2, 3, 'CAM-123');

TRUNCATE TABLE unit_workdays CASCADE;
INSERT INTO unit_workdays VALUES(1, '2019-12-02', 1, 1, 2, 3);

TRUNCATE TABLE v1.parents CASCADE;
INSERT INTO v1.parents VALUES(1, 'parent v1-1'), (2, 'parent v1-2');

TRUNCATE TABLE v2.parents CASCADE;
INSERT INTO v2.parents VALUES(3, 'parent v2-3'), (4, 'parent v2-4');

TRUNCATE TABLE v2.another_table CASCADE;
INSERT INTO v2.another_table VALUES(5, 'value 5'), (6, 'value 6');

TRUNCATE TABLE shops CASCADE;
INSERT INTO shops(id, address, shop_geom) VALUES(1, '1369 Cambridge St', '0101000020E610000075CDE49B6DC651C0E0D6DD3CD52F4540');
INSERT INTO shops(id, address, shop_geom) VALUES(2, '757 Massachusetts Ave', '0101000020E61000001B81785DBFC651C09C1A683EE72E4540');
INSERT INTO shops(id, address, shop_geom) VALUES(3, '605 W Kendall St', '0101000020E61000007B30293E3EC551C05FD218ADA32E4540');
