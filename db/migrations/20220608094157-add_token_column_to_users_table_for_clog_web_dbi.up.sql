-- id: 20220608094157
-- direction: UP
-- description: add_token_column_to_users_table_for_clog_web_dbi
ALTER TABLE users
  ADD COLUMN TOKEN VARCHAR NOT NULL DEFAULT gen_random_uuid();
