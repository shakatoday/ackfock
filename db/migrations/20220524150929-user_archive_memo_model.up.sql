-- id: 20220524150929
-- direction: UP
-- description: user_archive_memo_model
CREATE TABLE "public"."archive" (
  uuid UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR NOT NULL DEFAULT 'unnamed',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
--;;
CREATE TABLE "public"."user_archive_access" (
  user_id UUID NOT NULL REFERENCES "public"."users" (UUID),
  archive_id UUID NOT NULL, -- will add foreign key constraint later in this migration
  PRIMARY KEY (user_id, archive_id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
--;;
CREATE TABLE "public"."user_ackfock" (
  user_id UUID NOT NULL REFERENCES "public"."users" (UUID),
  memo_id UUID NOT NULL REFERENCES "public"."memo" (UUID),
  ackfock ackfock NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (user_id, memo_id, created_at)
);
--;;
ALTER TABLE "public"."memo"
  ADD COLUMN archive_id UUID;
--;;
UPDATE "public"."memo"
   SET archive_id = gen_random_uuid();
--;;
INSERT INTO "public"."archive" (uuid)
SELECT archive_id
  FROM "public"."memo";
--;;
INSERT INTO "public"."user_archive_access" (user_id, archive_id)
SELECT source_user_id, archive_id
  FROM "public"."memo";
--;;
INSERT INTO "public"."user_archive_access" (user_id, archive_id)
SELECT target_user_id, archive_id
  FROM "public"."memo"
 WHERE target_user_id IS NOT NULL;
--;;
INSERT INTO "public"."user_ackfock" (user_id, memo_id, ackfock)
SELECT source_user_id, uuid, source_user_ackfock
  FROM "public"."memo"
 WHERE source_user_ackfock IS NOT NULL;
--;;
INSERT INTO "public"."user_ackfock" (user_id, memo_id, ackfock)
SELECT target_user_id, uuid, target_user_ackfock
  FROM "public"."memo"
 WHERE (target_user_id, target_user_ackfock) IS NOT NULL;
--;;
ALTER TABLE "public"."user_archive_access"
ADD FOREIGN KEY (archive_id) REFERENCES "public"."archive" (UUID);
--;;
ALTER TABLE "public"."memo"
ADD FOREIGN KEY (archive_id) REFERENCES "public"."archive" (UUID);
