SELECT
  ci.chat_item_id, ci.creating_message_id, ci.item_type, ci.item_text, ci.item_props,
  cic.content_type, cic.content,
  dci.item_sent, dci.item_ts, dci.item_deleted,
  cp.display_name, cp.full_name, cp.properties
FROM chat_items ci
JOIN chat_item_content cic ON cic.chat_item_id == ci.chat_item_id
JOIN direct_chat_items dci ON dci.chat_item_id == ci.chat_item_id
JOIN contacts c ON c.contact_id == dci.contact_id
JOIN contact_profiles cp ON cp.contact_profile_id == c.contact_profile_id
WHERE c.user_id = ? AND c.contact_id = ?
ORDER BY item_ts DESC;
