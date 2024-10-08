-- id: 20220319215648
-- direction: UP
-- description: create_ackfock_type_user_table_memo_table_and_updated_at_trigger
CREATE TYPE ackfock AS ENUM ('ACK', 'FOCK');
--;;
CREATE TABLE "public"."user" (
  uuid UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR UNIQUE NOT NULL,
  username VARCHAR UNIQUE NOT NULL,
  password_salted VARCHAR NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
--;;
CREATE TABLE "public"."memo" (
  uuid UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  source_user_id UUID NOT NULL REFERENCES "public"."user" (uuid),
  target_user_id UUID REFERENCES "public"."user" (UUID),
  CONTENT TEXT NOT NULL DEFAULT '',
  source_user_ackfock ackfock,
  target_user_ackfock ackfock,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
--;;
CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
--;;
CREATE TRIGGER set_timestamp
BEFORE UPDATE ON "public"."user"
FOR EACH ROW
EXECUTE FUNCTION trigger_set_timestamp();
--;;
CREATE TRIGGER set_timestamp
BEFORE UPDATE ON "public"."memo"
FOR EACH ROW
EXECUTE FUNCTION trigger_set_timestamp();
