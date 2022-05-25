-- id: 20220525161322
-- direction: UP
-- description: user_archive_memo_model_drop_columns
ALTER TABLE "public"."memo"
  DROP COLUMN source_user_id,
  DROP COLUMN target_user_id,
  DROP COLUMN source_user_ackfock,
  DROP COLUMN target_user_ackfock;
