% Record defineing the stream table in the database

-record(stream, {id,
		 type,
		 latitude,
		 longitude,
		 description,
		 public_access,
		 public_search,
		 frozen,
		 history_size,
		 last_updated,
		 secret_key,
		 owner_id,
		 resource_id,
		 version}).