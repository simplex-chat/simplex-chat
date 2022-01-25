SELECT
  ci.chat_item_id, ci.creating_message_id, ci.item_type, ci.item_text, ci.item_props,
  cic.content_type, cic.content,
  gci.item_sent, gci.item_ts, gci.item_deleted,
  gp.display_name, gp.full_name, gp.properties,
  cp.display_name, cp.full_name, cp.properties
FROM chat_items ci
JOIN chat_item_content cic ON cic.chat_item_id == ci.chat_item_id
JOIN group_chat_items gci ON gci.chat_item_id == ci.chat_item_id
JOIN groups g ON g.group_id == gci.group_id
JOIN group_profiles gp ON gp.group_profile_id == g.group_profile_id
JOIN group_members ON gm.group_member_id == gci.group_member_id
JOIN contact_profiles cp ON cp.contact_profile_id == gm.contact_profile_id
WHERE g.user_id = ? AND g.group_id = ?
ORDER BY item_ts DESC;
