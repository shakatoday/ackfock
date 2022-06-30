-- id: 20220630120931
-- direction: DOWN
-- description: drop_updated_at_column_of_memo
ALTER TABLE memo
  ADD COLUMN updated_at TIMESTAMPTZ NOT NULL DEFAULT now();
--;;
CREATE TRIGGER set_timestamp
BEFORE UPDATE ON "public"."memo"
FOR EACH ROW
EXECUTE FUNCTION trigger_set_timestamp();
