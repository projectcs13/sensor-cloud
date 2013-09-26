% Record defining the stream table in the database

-record(resource, {id,
		   label,
		   version,
		   owner_id,
		   parent_id,
		   polling_url,
		   polling_authentication_key,
		   polling_period,
		   secret_key,
		   description,
		   last_polled,
		   last_posted}).
