DROP INDEX "user_username_key";
ALTER TABLE "account" DROP COLUMN "token";
ALTER TABLE "account" ALTER COLUMN "updated_at" TYPE timestamp with time zone, ALTER COLUMN "updated_at" DROP NOT NULL;
