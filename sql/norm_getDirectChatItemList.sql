SELECT
  ci.chat_item_id, ci.chat_msg_id, ci.created_by_message_id, ci.item_text, ci.item_content,
  dci.item_sent, dci.item_ts, dci.item_deleted,
  cp.display_name, cp.full_name, cp.properties
FROM chat_items ci
JOIN direct_chat_items dci ON dci.chat_item_id == ci.chat_item_id
JOIN contacts c ON c.contact_id == dci.contact_id
JOIN contact_profiles cp ON cp.contact_profile_id == c.contact_profile_id
WHERE c.user_id = ? AND c.contact_id = ?
ORDER BY item_ts DESC;
