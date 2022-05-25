-- id: 20220524150929
-- direction: DOWN
-- description: user_archive_memo_model
DROP TABLE "public"."user_ackfock";
--;;
ALTER TABLE "public"."memo"
  DROP COLUMN archive_id;
--;;
DROP TABLE "public"."user_archive_access";
--;;
DROP TABLE "public"."archive";
