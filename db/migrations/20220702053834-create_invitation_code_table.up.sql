-- id: 20220702053834
-- direction: UP
-- description: create_invitation_code_table
CREATE TABLE invitation_code (
  code VARCHAR(255) PRIMARY KEY,
  source_user_id UUID NOT NULL REFERENCES users (UUID),
  used_by_user_id UUID REFERENCES users (UUID),
  channel_id UUID NOT NULL REFERENCES channel (UUID),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  valide_until TIMESTAMPTZ NOT NULL
);

