SELECT
  chat_type,
  chat_item_id, creating_message_id, item_type, item_text, item_props,
  content_type, content,
  item_sent, item_ts, item_deleted,
  group_display_name, group_full_name, group_properties,
  contact_display_name, contact_full_name, contact_properties
FROM (
  SELECT
      'direct' AS chat_type,
      ci.chat_item_id AS chat_item_id, ci.creating_message_id AS creating_message_id, ci.item_type AS item_type, ci.item_text AS item_text, ci.item_props AS item_props,
      cic.content_type AS content_type, cic.content AS content,
      dci.item_sent AS item_sent, dci.item_ts AS item_ts, dci.item_deleted AS item_deleted,
      NULL AS group_display_name, NULL AS group_full_name, NULL AS group_properties,
      cp.display_name AS contact_display_name, cp.full_name AS contact_full_name, cp.properties AS contact_properties
    FROM chat_items ci
    JOIN chat_item_content cic ON cic.chat_item_id == ci.chat_item_id
    JOIN (
      SELECT contact_id, chat_item_id, MAX(item_ts) MaxDate
      FROM direct_chat_items
      WHERE item_deleted != 1
      GROUP BY contact_id, chat_item_id
    ) DCIMaxDates ON DCIMaxDates.chat_item_id = ci.chat_item_id
    JOIN direct_chat_items dci ON dci.chat_item_id == DCIMaxDates.chat_item_id
                              AND dci.item_ts == DCIMaxDates.item_ts
    JOIN contacts c ON c.contact_id == dci.contact_id
    JOIN contact_profiles cp ON cp.contact_profile_id == c.contact_profile_id
    WHERE c.user_id = ?
  UNION
  SELECT
      'group' AS chat_type,
      ci.chat_item_id AS chat_item_id, ci.creating_message_id AS creating_message_id, ci.item_type AS item_type, ci.item_text AS item_text, ci.item_props AS item_props,
      cic.content_type AS content_type, cic.content AS content,
      gci.item_sent AS item_sent, gci.item_ts AS item_ts, gci.item_deleted AS item_deleted,
      gp.display_name AS group_display_name, gp.full_name AS group_full_name, gp.properties AS group_properties,
      cp.display_name AS contact_display_name, cp.full_name AS contact_full_name, cp.properties AS contact_properties
    FROM chat_items ci
    JOIN chat_item_content cic ON cic.chat_item_id == ci.chat_item_id
    JOIN (
      SELECT group_id, chat_item_id, MAX(item_ts) MaxDate
      FROM group_chat_items
      WHERE item_deleted != 1
      GROUP BY group_id, chat_item_id
    ) GCIMaxDates ON GCIMaxDates.chat_item_id = ci.chat_item_id
    JOIN group_chat_items gci ON gci.chat_item_id == GCIMaxDates.chat_item_id
                              AND gci.item_ts == GCIMaxDates.item_ts
    JOIN groups g ON g.group_id == gci.group_id
    JOIN group_profiles gp ON gp.group_profile_id == g.group_profile_id
    JOIN group_members ON gm.group_member_id == gci.group_member_id
    JOIN contact_profiles cp ON cp.contact_profile_id == gm.contact_profile_id
    WHERE g.user_id = ?
)
ORDER BY item_ts DESC;
