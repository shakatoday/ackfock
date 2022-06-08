-- id: 20220608074501
-- direction: UP
-- description: create_content_table_for_clog_web_dbi
create table CONTENT (
  key VARCHAR,
  value varchar,
  parent VARCHAR,
  title VARCHAR,
  username VARCHAR,
  createdate TIMESTAMPTZ DEFAULT now()
);
--;;
INSERT INTO CONTENT
VALUES ('main', 'Welcome to Ackfock', NULL, 'Welcome to Ackfock', NULL, DEFAULT),
       ('about','All about this site.', NULL, 'About', NULL, DEFAULT);
