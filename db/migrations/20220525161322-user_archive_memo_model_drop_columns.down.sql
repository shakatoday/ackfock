-- id: 20220525161322
-- direction: DOWN
-- description: user_archive_memo_model_drop_columns
ALTER TABLE "public"."memo"
  ADD COLUMN source_user_id UUID REFERENCES users(UUID),
  ADD COLUMN target_user_id UUID REFERENCES users(UUID),
  ADD COLUMN source_user_ackfock ackfock,
  ADD COLUMN target_user_ackfock ackfock;
--;;
UPDATE memo
   SET source_user_id = (SELECT user_id
                           FROM user_archive_access
                          WHERE user_archive_access.archive_id = memo.archive_id
                          LIMIT 1);
--;;
UPDATE memo
   SET target_user_id = (SELECT user_id
                           FROM user_archive_access
                          WHERE memo.archive_id = user_archive_access.archive_id AND user_archive_access.user_id != memo.source_user_id);
--;;
UPDATE memo
   SET target_user_ackfock = (SELECT ackfock
                                FROM user_ackfock
                               WHERE user_ackfock.user_id = memo.target_user_id AND user_ackfock.memo_id = memo.UUID),
       source_user_ackfock = (SELECT ackfock
                                FROM user_ackfock
                               WHERE user_ackfock.user_id = memo.source_user_id AND user_ackfock.memo_id = memo.UUID);
--;;
ALTER TABLE "public"."memo"
  ALTER COLUMN source_user_id SET NOT NULL;
