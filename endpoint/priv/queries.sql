-- :insert_new_instance
INSERT INTO instances (instance_id, user_id, inserted_at, updated_at) VALUES ($1, $2, now(), now());

-- :select_instances_for_user_id
SELECT instance_id FROM instances WHERE user_id = $1;
