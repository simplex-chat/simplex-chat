ALTER TABLE group_members ADD inv_queue_info BLOB;

CREATE INDEX idx_groups_inv_queue_info ON groups (inv_queue_info);
