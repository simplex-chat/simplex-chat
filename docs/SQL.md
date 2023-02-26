| Updated 26.02.2023 | Languages: EN, [FR](/docs/lang/fr/SQL.md), [CZ](/docs/lang/cs/SQL.md) |

# Accessing messages in the database

## Decrypting databases

In order to view database data you need to decrypt it first. Install `sqlcipher` using your favorite package manager and run the following commands in the directory with databases:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Ensure it works fine
select * from users;
```

If you see `Parse error: no such table: users`, make sure you entered correct passphrase, and you have changed passphrase from random in Android app (if you got this database from Android device, of course).

## SQL queries

You can run queries against `direct_messages`, `group_messages` and `all_messages` (or their simpler alternatives `direct_messages_plain`, `group_messages_plain` and `all_messages_plain`), for example:

```sql
-- you can put these or your preferred settings into ~/.sqliterc
-- to persist across sqlite3 client sessions
.mode column
.headers on
.nullvalue NULL

-- simple views into direct, group and all_messages
-- with user's messages deduplicated for group and all_messages;
-- only 'x.msg.new' ("new message") chat events - filters out service events;
-- msg_sent is 0 for received, 1 for sent
select * from direct_messages_plain;
select * from group_messages_plain;
select * from all_messages_plain;

-- query other details of your chat history with regular SQL, for example:
-- files you offered for sending
select * from direct_messages where msg_sent = 1 and chat_msg_event = 'x.file';
-- everything catherine sent related to cats
select * from direct_messages where msg_sent = 0 and contact = 'catherine' and msg_body like '%cats%';
-- all correspondence with alice in #team
select * from group_messages where group_name = 'team' and contact = 'alice';

-- aggregate your chat data
select contact_or_group, num_messages from (
  select
    contact as contact_or_group, count(1) as num_messages
    from direct_messages_plain group by contact
  union
  select
    group_name as contact_or_group, count(1) as num_messages
    from group_messages_plain group by group_name
)
order by num_messages desc;
```
