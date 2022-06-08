-- id: 20220608115109
-- direction: DOWN
-- description: rename_column_password_salted_to_password
ALTER TABLE users
  RENAME COLUMN PASSWORD TO password_salted;
