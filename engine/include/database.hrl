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

% Record defineing the stream table in the database

-record(user, {id, 
	       email,
	       user_name,
	       first_name,
	       last_name,
	       desciption,
	       latitude,
	       longitude,
	       creation_date,
	       last_login}).

% Record defineing the stream table in the database

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


