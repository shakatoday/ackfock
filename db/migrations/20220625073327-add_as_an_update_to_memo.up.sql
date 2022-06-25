-- id: 20220625073327
-- direction: UP
-- description: add_as_an_update_to_memo
ALTER TABLE  memo
  ADD COLUMN as_an_update BOOLEAN NOT NULL DEFAULT FALSE;
--;;
ALTER TABLE memo
  ADD CHECK (NOT (parent_memo_id IS NULL AND as_an_update = true));
