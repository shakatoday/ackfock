-- id: 20220414163014
-- direction: UP
-- description: create_authentication_code_table
CREATE TABLE "public"."authentication_code" (
  code VARCHAR(255) PRIMARY KEY,
  email VARCHAR NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  valid_until TIMESTAMPTZ NOT NULL
);
