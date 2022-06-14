-- id: 20220614135320
-- direction: DOWN
-- description: rename_archive_to_channel
ALTER TABLE channel RENAME TO archive;
--;;
ALTER TABLE user_channel_access RENAME TO user_archive_access;
--;;
ALTER TABLE user_archive_access
  RENAME COLUMN channel_id TO archive_id;
--;;
ALTER TABLE memo
  RENAME COLUMN channel_id TO archive_id;
