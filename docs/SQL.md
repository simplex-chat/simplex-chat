---
title: Accessing messages in the database
revision: 25.09.2023
---

| Updated 25.09.2023 | Languages: EN, [FR](/docs/lang/fr/SQL.md), [CZ](/docs/lang/cs/SQL.md) |

# Accessing Messages in the Database

## Decrypting Databases

In order to view database data you need to decrypt it first. Install [`sqlcipher`](https://github.com/sqlcipher/sqlcipher) using your favorite package manager, but make sure its version is **at least** `4.5.0`!

You can check the version by running the binary on its own and then read the version string, which might, for example, look like this.

```
SQLite version 3.36.0 2021-06-18 18:36:39 (SQLCipher 4.5.0 community)
```

Then, run the following commands in the directory with the backup archive.

Unpack the archive.
```bash
unzip simplex-chat.2023-09-25T023823.zip
```

Run this in your shell, to enter the SQLite CLI.
```bash
sqlcipher "simplex_v1_chat.db"
```

Execute the following statements inside the SQLite CLI we just opened.
First, you set the decryption passphrase for the database. You had set this, when running SimpleX for the first time.
The following `SELECT` command checks, whether the decryption works.
```sql
PRAGMA KEY = 'yourDecryptionPassphrase';
# Ensure it works fine.
SELECT * FROM users;
```

If you see `Parse error: no such table: users` or `Error: file is encrypted or is not a database`, make sure you entered the correct passphrase and your SQLCipher version is at least version `4.5.0`.

## SQL Queries

To see all tables available in this database, run `.tables`.

```
sqlite> .tables
calls                         known_servers
chat_item_messages            messages
chat_item_moderations         migrations
chat_item_reactions           msg_deliveries
chat_item_versions            msg_delivery_events
chat_items                    pending_group_messages
commands                      protocol_servers
connections                   rcv_file_chunks
contact_profiles              rcv_files
contact_requests              received_probes
contacts                      sent_probe_hashes
display_names                 sent_probes
extra_xftp_file_descriptions  settings
files                         snd_file_chunks
group_member_intros           snd_files
group_members                 user_contact_links
group_profiles                users
groups                        xftp_file_descriptions
sqlite>
```

Prepare the result view.

```sql
.mode column
.headers on
.nullvalue NULL
```

You may also put these options and preferred settings into `~/.sqliterc` to persist them across SQLite client sessions.

To view the last message from anyone, which has the word "like" in it, run the following query.

```sql
SELECT * FROM messages where msg_body like '%like%' ORDER BY ROWID DESC LIMIT 1;
```

View the last ten messages, which are not simply a new text message.

```sql
SELECT * FROM messages WHERE chat_msg_event LIKE '%msg%' AND chat_msg_event NOT LIKE '%new%' ORDER BY ROWID DESC LIMIT 10;
```

View all contacts.

```sql
SELECT contact_id, contact_profile_id, user_id, local_display_name FROM contacts WHERE is_user = '0';
```

Note the `contact_id` of the user you are interested in and replace `<CONTACT_ID>` with it in the following commands.

Load 30 most recent messages from your chosen contact, newest to oldest.

```sql
SELECT
    chat_item_id,
    item_text
  FROM chat_items
    WHERE created_by_msg_id
      IN (
        SELECT message_id
          FROM messages
            WHERE msg_sent = '0'
            AND NULLIF(message_id, '') IS NOT NULL
      )
    AND contact_id = '<CONTACT_ID>'
    AND NULLIF(item_text, '') IS NOT NULL
    ORDER BY ROWID DESC LIMIT 30;
```

Load the first 30 messages from your chosen contact, oldest to newest.

```sql
SELECT
    chat_item_id,
    item_text
  FROM chat_items
    WHERE created_by_msg_id
      IN (
        SELECT message_id
          FROM messages
            WHERE msg_sent = '0'
            AND NULLIF(message_id, '') IS NOT NULL
      )
    AND contact_id = '<CONTACT_ID>'
    AND NULLIF(item_text, '') IS NOT NULL
    ORDER BY ROWID ASC LIMIT 30;
```
Load ALL messages from your chosen contact, oldest to newest.

```sql
SELECT
    chat_item_id,
    item_text
  FROM chat_items
    WHERE created_by_msg_id
      IN (
        SELECT message_id
          FROM messages
            WHERE msg_sent = '0'
            AND NULLIF(message_id, '') IS NOT NULL
      )
    AND contact_id = '<CONTACT_ID>'
    AND NULLIF(item_text, '') IS NOT NULL
    ORDER BY ROWID ASC;
```
Load ALL messages from your chosen contact, which contain the word "like" in its text content, oldest to newest.

```sql
SELECT
    chat_item_id,
    item_text
  FROM chat_items
    WHERE created_by_msg_id
      IN (
        SELECT message_id
          FROM messages
            WHERE msg_sent = '0'
            AND NULLIF(message_id, '') IS NOT NULL
      )
    AND contact_id = '<CONTACT_ID>'
    AND NULLIF(item_text, '') IS NOT NULL
    AND item_text LIKE '%like%'
    ORDER BY ROWID ASC;
```