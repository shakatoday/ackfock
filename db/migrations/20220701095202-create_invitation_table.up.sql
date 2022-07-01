-- id: 20220701095202
-- direction: UP
-- description: create_invitation_table
CREATE TABLE invitation (
  source_user_id UUID NOT NULL REFERENCES users (UUID),
  email VARCHAR NOT NULL,
  channel_id UUID NOT NULL REFERENCES channel (UUID),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  valid_until TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (source_user_id, email, created_at)
);
