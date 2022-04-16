# Accessing message history via SQL queries

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
