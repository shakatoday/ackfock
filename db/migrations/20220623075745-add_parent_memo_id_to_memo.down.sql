-- id: 20220623075745
-- direction: DOWN
-- description: add_parent_memo_id_to_memo
ALTER TABLE memo
  DROP COLUMN parent_memo_id;
