-- id: 20221004025349
-- direction: DOWN
-- description: drop_trigger_on_account
CREATE TRIGGER set_timestamp
BEFORE UPDATE ON account
FOR EACH ROW
EXECUTE FUNCTION trigger_set_timestamp();
