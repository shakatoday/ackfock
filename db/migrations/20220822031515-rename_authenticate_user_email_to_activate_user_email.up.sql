-- id: 20220822031515
-- direction: UP
-- description: rename_authenticate_user_email_to_activate_user_email
ALTER TABLE authentication_code RENAME TO activation_code;
--;;
ALTER TABLE users
  RENAME COLUMN email_authenticated_at TO email_activated_at;
