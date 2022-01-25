SELECT
  c.contact_id,
  cp.display_name,
  cp.full_name,
  cp.properties,
  g.group_id,
  gp.display_name,
  gp.full_name,
  gp.properties,
  gm.group_member_id,
  gmp.display_name,
  gmp.full_name,
  gmp.properties,
  ci.chat_item_id,
  ci.chat_msg_id,
  ci.created_by_message_id,
  ci.item_sent,
  ci.item_ts,
  ci.item_deleted,
  ci.item_text,
  ci.item_content,
  md.msg_delivery_id,
  md.chat_ts,
  md.agent_msg_meta,
  mde.delivery_status,
  mde.created_at
FROM chat_items ci
LEFT JOIN contacts c ON c.contact_id == ci.contact_id
JOIN contact_profiles cp ON cp.contact_profile_id == c.contact_profile_id
LEFT JOIN groups g ON g.group_id = ci.group_id
JOIN group_profiles gp ON gp.group_profile_id == g.group_profile_id
LEFT JOIN group_members ON gm.group_member_id == ci.group_member_id
JOIN contact_profiles gmp ON gmp.contact_profile_id == gm.contact_profile_id
JOIN messages m ON m.message_id == ci.created_by_message_id
JOIN msg_deliveries md ON md.message_id = m.message_id
JOIN (
  SELECT msg_delivery_id, MAX(created_at) MaxDate
  FROM msg_delivery_events
  GROUP BY msg_delivery_id
) MDEMaxDates ON MDEMaxDates.msg_delivery_id = md.msg_delivery_id
JOIN msg_delivery_events mde ON mde.msg_delivery_id = MDEMaxDates.msg_delivery_id
                            AND mde.created_at = MDEMaxDates.MaxDate
WHERE ci.user_id = ?
ORDER BY ci.item_ts DESC
