CREATE VIEW direct_messages AS
SELECT
  ct.local_display_name AS contact,
  m.message_id AS message_id,
  m.msg_sent AS msg_sent,
  m.chat_msg_event AS chat_msg_event,
  m.msg_body AS msg_body,
  md.msg_delivery_id AS delivery_id,
  datetime(md.chat_ts) AS chat_dt,
  md.agent_msg_meta AS msg_meta,
  mde.delivery_status AS delivery_status,
  datetime(mde.created_at) AS delivery_status_dt
FROM messages m
JOIN msg_deliveries md ON md.message_id = m.message_id
JOIN (
  SELECT msg_delivery_id, MAX(created_at) MaxDate
  FROM msg_delivery_events
  GROUP BY msg_delivery_id
) MaxDates ON MaxDates.msg_delivery_id = md.msg_delivery_id
JOIN msg_delivery_events mde ON mde.msg_delivery_id = MaxDates.msg_delivery_id
                            AND mde.created_at = MaxDates.MaxDate
JOIN connections c ON c.connection_id = md.connection_id
JOIN contacts ct ON ct.contact_id = c.contact_id
ORDER BY chat_dt DESC;

CREATE VIEW direct_messages_plain AS
SELECT
  dm.contact AS contact,
  dm.msg_sent AS msg_sent,
  dm.msg_body AS msg_body,
  dm.chat_dt AS chat_dt
FROM direct_messages dm
WHERE dm.chat_msg_event = 'x.msg.new';

CREATE VIEW group_messages AS
SELECT
  g.local_display_name AS group_name,
  gm.local_display_name AS contact,
  m.message_id AS message_id,
  m.msg_sent AS msg_sent,
  m.chat_msg_event AS chat_msg_event,
  m.msg_body AS msg_body,
  md.msg_delivery_id AS delivery_id,
  datetime(md.chat_ts) AS chat_dt,
  md.agent_msg_meta AS msg_meta,
  mde.delivery_status AS delivery_status,
  datetime(mde.created_at) AS delivery_status_dt
FROM messages m
JOIN msg_deliveries md ON md.message_id = m.message_id
JOIN (
  SELECT msg_delivery_id, MAX(created_at) MaxDate
  FROM msg_delivery_events
  GROUP BY msg_delivery_id
) MaxDates ON MaxDates.msg_delivery_id = md.msg_delivery_id
JOIN msg_delivery_events mde ON mde.msg_delivery_id = MaxDates.msg_delivery_id
                            AND mde.created_at = MaxDates.MaxDate
JOIN connections c ON c.connection_id = md.connection_id
JOIN group_members gm ON gm.group_member_id = c.group_member_id
JOIN groups g ON g.group_id = gm.group_id
ORDER BY chat_dt DESC;

CREATE VIEW group_messages_plain AS
SELECT
  gm.group_name AS group_name,
  (CASE WHEN gm.msg_sent = 0 THEN gm.contact ELSE gm.group_name END) AS contact,
  gm.msg_sent AS msg_sent,
  gm.msg_body AS msg_body,
  gm.chat_dt AS chat_dt
FROM group_messages gm
JOIN (
  SELECT message_id, MIN(delivery_id) MinDeliveryId
  FROM group_messages
  GROUP BY message_id
) Deduplicated ON Deduplicated.message_id = gm.message_id
              AND Deduplicated.MinDeliveryId = gm.delivery_id
WHERE gm.chat_msg_event = 'x.msg.new';

CREATE VIEW all_messages (
  group_name,
  contact,
  message_id,
  msg_sent,
  chat_msg_event,
  msg_body,
  delivery_id,
  chat_dt,
  msg_meta,
  delivery_status,
  delivery_status_dt
) AS
  SELECT * FROM (
    SELECT NULL AS group_name, * FROM direct_messages
    UNION
    SELECT * FROM group_messages
  )
  ORDER BY chat_dt DESC;

CREATE VIEW all_messages_plain (
  group_name,
  contact,
  msg_sent,
  msg_body,
  chat_dt
) AS
  SELECT * FROM (
    SELECT NULL AS group_name, * FROM direct_messages_plain
    UNION
    SELECT * FROM group_messages_plain
  )
  ORDER BY chat_dt DESC;
