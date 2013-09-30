
-record(users, {id, email, user_name, first_name, last_name,
		description, latitude, longitude, creation_date, last_login}).

-record(resources, {id, label, version, owner_id,
		parent_id, polling_url, polling_authentication_key,
		polling_period, secret_key, description, last_polled,
		last_posted}).

-record(streams, {id, type, latitude, longitude, 
		  description, public_access, public_search,
		  frozen, history_size, last_updated,
		  secret_key, owner_id, resource_id, version}).

