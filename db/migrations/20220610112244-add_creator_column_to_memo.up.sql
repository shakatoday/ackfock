-- id: 20220610112244
-- direction: UP
-- description: add_creator_column_to_memo
ALTER TABLE memo
  ADD COLUMN creator_id UUID REFERENCES users (UUID);
--;;
UPDATE memo
   SET creator_id = (SELECT user_archive_access.user_id
                       FROM user_archive_access
                      WHERE user_archive_access.archive_id = memo.archive_id
                      LIMIT 1);
--;;
ALTER TABLE memo
  ALTER COLUMN creator_id SET NOT NULL;
