-- id: 20220822031515
-- direction: DOWN
-- description: rename_authenticate_user_email_to_activate_user_email
ALTER TABLE activation_code RENAME TO authentication_code;
--;;
ALTER TABLE users
  RENAME COLUMN email_activated_at TO email_authenticated_at;
