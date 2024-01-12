plan

-- direct last

SELECT
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
          -- DirectQuote
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
        FROM chat_items i
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
        ORDER BY i.created_at DESC, i.chat_item_id DESC
        LIMIT ?

EXPLAIN QUERY PLAN SELECT          i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,          f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent        FROM chat_items i        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id        LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id        WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'        ORDER BY i.created_at DESC, i.chat_item_id DESC        LIMIT ?;

-- direct after

SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id > ?
            ORDER BY i.created_at ASC, i.chat_item_id ASC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent            FROM chat_items i            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'              AND i.chat_item_id > ?            ORDER BY i.created_at ASC, i.chat_item_id ASC            LIMIT ?;

-- direct before

SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id < ?
            ORDER BY i.created_at DESC, i.chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent            FROM chat_items i            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'              AND i.chat_item_id < ?            ORDER BY i.created_at DESC, i.chat_item_id DESC            LIMIT ?;

-- local last

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY created_at DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'            ORDER BY created_at DESC, chat_item_id DESC            LIMIT ?;

-- local after

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'
              AND chat_item_id > ?
            ORDER BY created_at ASC, chat_item_id ASC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'              AND chat_item_id > ?            ORDER BY created_at ASC, chat_item_id ASC            LIMIT ?;

-- new

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'
              AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))
            ORDER BY created_at ASC, chat_item_id ASC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'              AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))            ORDER BY created_at ASC, chat_item_id ASC            LIMIT ?;

-- new 2

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND note_folder_id = ?
              AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))
              AND item_text LIKE '%' || ? || '%'
            ORDER BY created_at ASC, chat_item_id ASC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND note_folder_id = ?              AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))              AND item_text LIKE '%' || ? || '%'            ORDER BY created_at ASC, chat_item_id ASC            LIMIT ?;

-- local before

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'
              AND chat_item_id < ?
            ORDER BY created_at DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE '%' || ? || '%'              AND chat_item_id < ?            ORDER BY created_at DESC, chat_item_id DESC            LIMIT ?;




-- groups logic is ok (?), but indexes don't use item_ts

-- groups last

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'            ORDER BY item_ts DESC, chat_item_id DESC            LIMIT ?;

-- groups after

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts > ? OR (item_ts = ? AND chat_item_id > ?))
            ORDER BY item_ts ASC, chat_item_id ASC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'              AND (item_ts > ? OR (item_ts = ? AND chat_item_id > ?))            ORDER BY item_ts ASC, chat_item_id ASC            LIMIT ?;

-- groups before

SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?

EXPLAIN QUERY PLAN SELECT chat_item_id            FROM chat_items            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))            ORDER BY item_ts DESC, chat_item_id DESC            LIMIT ?;


-- also check previews

-- direct last

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

-- direct before

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

-- local last

SELECT nf.note_folder_id, nf.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), nf.unread_chat
        FROM note_folders nf
        LEFT JOIN (
          SELECT note_folder_id, chat_item_id, MAX(created_at)
          FROM chat_items
          GROUP BY note_folder_id
        ) LastItems ON LastItems.note_folder_id = nf.note_folder_id
        LEFT JOIN (
          SELECT note_folder_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY note_folder_id
        ) ChatStats ON ChatStats.note_folder_id = nf.note_folder_id
        WHERE nf.user_id = :user_id
        ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT nf.note_folder_id, nf.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), nf.unread_chat        FROM note_folders nf        LEFT JOIN (          SELECT note_folder_id, chat_item_id, MAX(created_at)          FROM chat_items          GROUP BY note_folder_id        ) LastItems ON LastItems.note_folder_id = nf.note_folder_id        LEFT JOIN (          SELECT note_folder_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY note_folder_id        ) ChatStats ON ChatStats.note_folder_id = nf.note_folder_id        WHERE nf.user_id = :user_id        ORDER BY ts DESC LIMIT :count;

-- local before

SELECT nf.note_folder_id, nf.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), nf.unread_chat
        FROM note_folders nf
        LEFT JOIN (
          SELECT note_folder_id, chat_item_id, MAX(created_at)
          FROM chat_items
          GROUP BY note_folder_id
        ) LastItems ON LastItems.note_folder_id = nf.note_folder_id
        LEFT JOIN (
          SELECT note_folder_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY note_folder_id
        ) ChatStats ON ChatStats.note_folder_id = nf.note_folder_id
        WHERE nf.user_id = :user_id
        AND ts < :ts ORDER BY ts DESC LIMIT :count

EXPLAIN QUERY PLAN SELECT nf.note_folder_id, nf.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), nf.unread_chat        FROM note_folders nf        LEFT JOIN (          SELECT note_folder_id, chat_item_id, MAX(created_at)          FROM chat_items          GROUP BY note_folder_id        ) LastItems ON LastItems.note_folder_id = nf.note_folder_id        LEFT JOIN (          SELECT note_folder_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread          FROM chat_items          WHERE item_status = :rcv_new          GROUP BY note_folder_id        ) ChatStats ON ChatStats.note_folder_id = nf.note_folder_id        WHERE nf.user_id = :user_id        AND ts < :ts ORDER BY ts DESC LIMIT :count;
