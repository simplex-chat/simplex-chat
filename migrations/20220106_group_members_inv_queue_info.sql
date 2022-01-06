ALTER TABLE group_members ADD inv_queue_info BLOB;

CREATE INDEX idx_group_members_inv_queue_info ON group_members (inv_queue_info);
