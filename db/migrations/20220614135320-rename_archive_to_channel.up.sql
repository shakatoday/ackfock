-- id: 20220614135320
-- direction: UP
-- description: rename_archive_to_channel
ALTER TABLE archive RENAME TO channel;
--;;
ALTER TABLE user_archive_access RENAME TO user_channel_access;
--;;
ALTER TABLE user_channel_access
  RENAME COLUMN archive_id TO channel_id;
--;;
ALTER TABLE memo
  RENAME COLUMN archive_id TO channel_id;
