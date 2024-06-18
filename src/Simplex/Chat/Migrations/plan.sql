plan

-- direct read

UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND contact_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?

EXPLAIN QUERY PLAN UPDATE chat_items SET item_status = ?, updated_at = ?          WHERE user_id = ? AND contact_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?;


UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND contact_id = ? AND item_status = ?

EXPLAIN QUERY PLAN UPDATE chat_items SET item_status = ?, updated_at = ?          WHERE user_id = ? AND contact_id = ? AND item_status = ?;

-- direct get chat
-- last

SELECT
      i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
      f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
      ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
    FROM chat_items i
    LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
    LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
    WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
    ORDER BY i.created_at DESC, i.chat_item_id DESC
    LIMIT ?

EXPLAIN QUERY PLAN SELECT      i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,      f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,      ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent    FROM chat_items i    LEFT JOIN files f ON f.chat_item_id = i.chat_item_id    LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id    WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'    ORDER BY i.created_at DESC, i.chat_item_id DESC    LIMIT ?;

-- before

SELECT
              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id < ?
            ORDER BY i.created_at DESC, i.chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent            FROM chat_items i            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'              AND i.chat_item_id < ?            ORDER BY i.created_at DESC, i.chat_item_id DESC            LIMIT ?;

-- direct get previews
-- last

SELECT ct.contact_id, ct.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat
        FROM contacts ct
        LEFT JOIN (
          SELECT contact_id, chat_item_id, MAX(created_at)
          FROM chat_items
          GROUP BY contact_id
        ) LastItems ON LastItems.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
        WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
        ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT ct.contact_id, ct.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat        FROM contacts ct        LEFT JOIN (          SELECT contact_id, chat_item_id, MAX(created_at)          FROM chat_items          GROUP BY contact_id        ) LastItems ON LastItems.contact_id = ct.contact_id        LEFT JOIN (          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY contact_id        ) ChatStats ON ChatStats.contact_id = ct.contact_id        WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used        ORDER BY ts DESC LIMIT :count;

-- before

SELECT ct.contact_id, ct.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat
        FROM contacts ct
        LEFT JOIN (
          SELECT contact_id, chat_item_id, MAX(created_at)
          FROM chat_items
          GROUP BY contact_id
        ) LastItems ON LastItems.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
        WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
        AND ts < :ts ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT ct.contact_id, ct.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat        FROM contacts ct        LEFT JOIN (          SELECT contact_id, chat_item_id, MAX(created_at)          FROM chat_items          GROUP BY contact_id        ) LastItems ON LastItems.contact_id = ct.contact_id        LEFT JOIN (          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY contact_id        ) ChatStats ON ChatStats.contact_id = ct.contact_id        WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used        AND ts < :ts ORDER BY ts DESC LIMIT :count;

-- group read

UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND group_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?

EXPLAIN QUERY PLAN UPDATE chat_items SET item_status = ?, updated_at = ?          WHERE user_id = ? AND group_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?;


UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND group_id = ? AND item_status = ?

EXPLAIN QUERY PLAN UPDATE chat_items SET item_status = ?, updated_at = ?          WHERE user_id = ? AND group_id = ? AND item_status = ?;

-- group get chat
-- last

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'            ORDER BY item_ts DESC, chat_item_id DESC            LIMIT ?;

-- before

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))            ORDER BY item_ts DESC, chat_item_id DESC            LIMIT ?;

-- get group previews
-- last

SELECT g.group_id, g.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat
        FROM groups g
        LEFT JOIN (
          SELECT group_id, chat_item_id, MAX(item_ts)
          FROM chat_items
          GROUP BY group_id
        ) LastItems ON LastItems.group_id = g.group_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
        WHERE g.user_id = :user_id
        ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT g.group_id, g.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat        FROM groups g        LEFT JOIN (          SELECT group_id, chat_item_id, MAX(item_ts)          FROM chat_items          GROUP BY group_id        ) LastItems ON LastItems.group_id = g.group_id        LEFT JOIN (          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY group_id        ) ChatStats ON ChatStats.group_id = g.group_id        WHERE g.user_id = :user_id        ORDER BY ts DESC LIMIT :count;

-- before

SELECT g.group_id, g.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat
        FROM groups g
        LEFT JOIN (
          SELECT group_id, chat_item_id, MAX(item_ts)
          FROM chat_items
          GROUP BY group_id
        ) LastItems ON LastItems.group_id = g.group_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
        WHERE g.user_id = :user_id
        AND ts < :ts ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT g.group_id, g.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat        FROM groups g        LEFT JOIN (          SELECT group_id, chat_item_id, MAX(item_ts)          FROM chat_items          GROUP BY group_id        ) LastItems ON LastItems.group_id = g.group_id        LEFT JOIN (          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY group_id        ) ChatStats ON ChatStats.group_id = g.group_id        WHERE g.user_id = :user_id        AND ts < :ts ORDER BY ts DESC LIMIT :count;
