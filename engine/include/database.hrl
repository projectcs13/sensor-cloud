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

%% @type user() = #user{id   = integer(),
%%						email = string(),
%%						user_name = string(),
%%						password = string(),
%%						first_name = string(),
%%						last_name = string(),
%%						description = string(),
%%                      latitude = string(),
%%                      longitude = string()
%%						creation_date = string()
%%						last_login = string()}
-record(user, {	id	:: integer(), 
	       		email :: string(),
	       		user_name :: string(),
				password :: string(),
	       		first_name :: string(),
	       		last_name :: string(),
	       		description :: string(),
	      		latitude :: string(),
	       		longitude :: string(),
	       		creation_date :: string(),
	       		last_login :: string()}).

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

% Record used for creating auto_increments in MNESIA
-record( unique_ids, {	type, 
						id} ).
