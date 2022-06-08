-- id: 20220608115109
-- direction: UP
-- description: rename_column_password_salted_to_password
ALTER TABLE users
  RENAME COLUMN password_salted TO PASSWORD;
