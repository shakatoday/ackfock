-- id: 20220413162335
-- direction: UP
-- description: email_authenticated_at
ALTER TABLE "public"."users"
  ADD COLUMN email_authenticated_at TIMESTAMPTZ;
