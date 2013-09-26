% Record defining the stream table in the database

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
