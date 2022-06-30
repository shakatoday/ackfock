-- id: 20220630120931
-- direction: UP
-- description: drop_updated_at_column_of_memo
ALTER TABLE memo
  DROP COLUMN updated_at;
--;;
DROP TRIGGER set_timestamp ON memo;
