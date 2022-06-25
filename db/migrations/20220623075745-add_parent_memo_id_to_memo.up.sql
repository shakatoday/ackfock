-- id: 20220623075745
-- direction: UP
-- description: add_parent_memo_id_to_memo
ALTER TABLE memo
  ADD COLUMN parent_memo_id UUID REFERENCES memo(UUID)
