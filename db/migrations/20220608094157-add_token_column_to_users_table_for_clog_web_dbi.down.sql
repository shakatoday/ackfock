-- id: 20220608094157
-- direction: DOWN
-- description: add_token_column_to_users_table_for_clog_web_dbi
ALTER TABLE users
  DROP COLUMN TOKEN;
