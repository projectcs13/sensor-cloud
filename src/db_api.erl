%% @author Tholsg�rd Gabriel
%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.2
%% @copyright [Copyright information]
%%
%% @doc == db_api ==
%% This module contains all the necessary functionality to connect
%% to a db and query it, erlang session will need to use the cookie
%% database when starting the erlang shell, this is done by adding the
%% flag "-setcookie database"
%%
%% @end

-module(db_api).
-include_lib("stdlib/include/qlc.hrl").
-include("include/user.hrl").
-include("include/resource.hrl").
-include("include/stream.hrl").
-include("include/unique_ids.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, add_stream/1, get_stream_by_id/1, get_all_streams/0,
		 update_stream/2, delete_stream_with_id/1, print_stream/1,
		 clear_stream_table/0]).

-export([connect/2,write_resource/1, traverse/1]).
-export([create_user/2, create_user/1, get_user_by_id/1, get_user_by_username/1, 
		authenticate/2, change_password/3, exists_username/1, get_all_users/0,
		update_user/2]).

-define(MNESIA_DIR, "/home/kristian/mnesia/").
-define(NODES, [node()]).


%% @doc
%% Function: write_resource/1
%% Purpose: used to add a new resource or update an exsisting one if a field has
%% 			value undefined then the old value is used
%% Returns: {atmoic,ok} | or {term(), term()}
%%
%% Side effects: Remotely writes the database at node given by get_local_db_node()
%% @end
-spec write_resource(Resource::record()) -> {atomic, ok} | {term(), term()}.

write_resource(Resource) ->
	Pattern = #resource{id = Resource#resource.id, _ = '_'},
	Fun1 = fun() ->
				   mnesia:match_object(Pattern)
		   end,
	{_,Result} = rpc:call(get_local_db_node(),mnesia,transaction, [Fun1]),
	if Result == [] ->
		   ResourceNew = Resource;
	   true ->
		   ResourceNew = combine(lists:nth(1, Result),Resource)
	end,
	Fun2 = fun() ->		
              mnesia:write(ResourceNew)
          end,
	rpc:call(get_local_db_node(),mnesia,transaction,[Fun2]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: combine/2
%% Purpose: Creates a new resource record from the given two
%% 			where the value in Resource1 is kept if the value
%%			in Resource2 is undefined
%% Returns: {atmoic,ok} | or {term(), term()}
%%
%% Side effects: Remotely writes the database at node given by get_local_db_node()
%% @end
-spec combine(Resource1::record(),Resource2::record()) -> record().

combine(Resource1,Resource2) ->
	#resource{id = case Resource2#resource.id of
				  		undefined -> Resource1#resource.id;
				  		_ -> Resource2#resource.id
			  	   end,
			  label = case Resource2#resource.label of
				  		   undefined -> Resource1#resource.label;
				  		   _ -> Resource2#resource.label
			  		  end,
			  version = case Resource2#resource.version of
				  			 undefined -> Resource1#resource.version;
				  			 _ -> Resource2#resource.version
			  			end,
			  owner_id = case Resource2#resource.owner_id of
				  			  undefined -> Resource1#resource.owner_id;
				  			  _ -> Resource2#resource.owner_id
			  			 end,
			  parent_id = case Resource2#resource.parent_id of
				  			   undefined -> Resource1#resource.parent_id;
				  			   _ -> Resource2#resource.parent_id
			  			  end,
			  polling_url = case Resource2#resource.polling_url of
				  				 undefined -> Resource1#resource.polling_url;
				  				 _ -> Resource2#resource.polling_url
			  				end,
			  polling_authentication_key = case Resource2#resource.polling_authentication_key of
				  								undefined -> Resource1#resource.polling_authentication_key;
				  								_ -> Resource2#resource.polling_authentication_key
			  							   end,
			  polling_period = case Resource2#resource.polling_period of
				  					undefined -> Resource1#resource.polling_period;
				  					_ -> Resource2#resource.polling_period
			  				   end,
			  secret_key = case Resource2#resource.secret_key of
				  				undefined -> Resource1#resource.secret_key;
				  				_ -> Resource2#resource.secret_key
			  			   end,
			  description = case Resource2#resource.description of
				  				 undefined -> Resource1#resource.description;
				  				 _ -> Resource2#resource.description
			  				end,
			  last_polled = case Resource2#resource.last_polled of
				  				 undefined -> Resource1#resource.last_polled;
				  				 _ -> Resource2#resource.last_polled
			  				end,
			  last_posted = case Resource2#resource.last_posted of
				  				 undefined -> Resource1#resource.last_posted;
				  				 _ -> Resource2#resource.last_posted
			  				end}.

%% @doc
%% Function: create_stream_table/0
%% Purpose: Create a table 'stream' in the database for streams.
%% Returns: {atomic, ok} | {aborted, Reason}
%%
%% Side effects: Create 'stream' table in the database following the
%%               specification of the record #stream.
%% @end
-spec create_stream_table() -> {atomic, ok} | {aborted, term()}.
create_stream_table() ->
  case mnesia:create_table(stream,
						 [{attributes, record_info(fields, stream)},
						{disc_copies, ?NODES},
						{type, ordered_set}])
	of
	{atomic, ok} -> {atomic, ok};
	{aborted, {already_exists, _}} -> {atomic, ok};
	{aborted, Reason} -> {aborted, Reason}
end.



%% @doc
%% Function: start/0
%% Purpose: Start a node for database connection.
%% Returns: ok | {error, Reason}
%%
%% Side effects: Starts a node for handling the database connection.
%%               Creates a database on disc.
%% @end
-spec start() -> ok | {error, term()}.
start() ->
	application:set_env(mnesia, dir, ?MNESIA_DIR),
	mnesia:create_schema(?NODES),
	application:start(mnesia),
	create_stream_table().



%% @doc
%% Function: stop/0
%% Purpose: Stops the node for the database connection.
%% Returns: ok | {error, Reason}
%%
%% @end
-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(mnesia).



%% @doc
%% Function: connect/2
%% Purpose: Establish a database connection
%% Returns: ok | or {error, term()}
%%
%% Side effects: Establishes a database connection
%% @end
-spec connect(string(), term()) -> {ok, term()} | {error, term()}.
connect(ConnectStr, Options) ->
	ok.

%% @doc
%% Function: createUser/2
%% Purpose: Inserts a new User to the system
%% Args:   string(), string()
%% Returns: ok | or {error, username_exists} | or {error, term()}
%%
%% Side effects: Creates a new User in the database
%% @end
-spec create_user(string(), string()) -> ok | {error, term()}.
create_user(Username, Password) ->	
	case exists_username(Username) of
		true -> {error, username_exists};
		false ->
			Id = mnesia:dirty_update_counter(unique_ids, user, 1),
			F = fun() ->
				mnesia:write(#user{id=Id, user_name=Username, password=Password})							
			end,
			mnesia:activity(transaction, F)
	end.

create_user(User) ->
	F = fun() ->
		mnesia:write(User)							
	end,
	mnesia:activity(transaction, F).


%% @doc
%% Function: exists_username/1
%% Purpose: Checks if username exists
%% Args:   string()
%% Returns: boolean()
%%
%% @end
-spec exists_username(string()) -> boolean().
exists_username(Username) ->	
	Pattern = #user{_ = '_',
					user_name = Username},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case mnesia:activity(transaction, F) of
		[] -> false;
		_ -> true
	end.



%% @doc
%% Function: get_user_by_id/1
%% Purpose: Retrieves a User from the system, given the ID
%% Args: integer()
%% Returns: user | {error, unknown_user}
%%
%% @end
-spec get_user_by_id(integer()) -> Record :: #user{}.
get_user_by_id(ID) ->
	Trans = fun() ->
					case mnesia:match_object(#user{id=ID, _='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "unknown_user"};
						[S|_] -> S
					end
			end,
	mnesia:wait_for_tables([user], 5000),
	mnesia:activity(transaction, Trans).

%% @doc
%% Function: getUserbyUsername/1
%% Purpose: Retrieves a User from the system, given the username
%% Args: string()
%% Returns: user | {error, unknown_user}
%%
%% @end
-spec get_user_by_username(string()) ->  Record :: #user{}.
get_user_by_username(Username) ->
	Pattern = #user{ _ = '_',
					 user_name = Username},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case User = mnesia:activity(transaction, F) of
		[] -> {error, unknown_user};
		_ -> User
	end.


%% @doc
%% Function: authenticate/2
%% Purpose: Authenticates a user
%% Args: string(), string()
%% Returns: ok | {error, authentication_error}
%%
%% @end
-spec authenticate(string(), string()) ->  ok | {error, term()}.
authenticate(Username,Password) ->
	Pattern = #user{_ = '_',
					user_name = Username,
					password = Password},
	F = fun() -> 
		mnesia:match_object(Pattern)
	end,
	case mnesia:activity(transaction, F) of
		[] -> {error, authentication_error};
		_ -> ok
	end.


%% @doc
%% Function: change_password/3
%% Purpose: Changes the user's password
%% Args: string(), string(), string()
%% Returns: ok | {error, username_password_wrong}
%%
%% @end
-spec change_password(string(), string(), string()) ->  ok | {error, term()}.
change_password(Username,OldPassword,NewPassword) ->
	Pattern = #user{_ = '_',
					user_name = Username,
					password = OldPassword},
	F = fun() -> 
		case mnesia:match_object(Pattern) of
			[User] -> mnesia:write(User#user{password = NewPassword});
			[] -> {error, username_password_wrong}
		end
	end,
	mnesia:activity(transaction, F).

%% @doc
%% Function: get_local_db_node/0
%% Purpose: returns the node name of the mnesia database
%% Returns: atom()
%%
%% @end
-spec get_local_db_node() -> atom().

get_local_db_node() ->
	{ok,Host} = inet:gethostname(),
    FullHost = string:concat("database@",Host), 
    HostAtom = list_to_atom(FullHost),
	HostAtom.










%% @doc
%% Function: traverse/2
%% Purpose: Prints a table, used for debug
%% Returns: ok | or {error, term()}
%%
%% Side effects: It prints the table on the screen
%% @end
traverse(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.







%% @doc
%% Function: add_stream/1
%% Purpose: Insert a stream in the database.
%% Returns: {aborted, Reason} | ok | exit(Reason)
%%
%% Side effects: Inserts a row in the table 'stream' in the database
%%               with the specified data.
%%               Id of the stream is set to the highest id in the table + 1.
%% @end
-spec add_stream(record) -> {aborted, term()} | ok | {error, term()}.
add_stream(Stream_record) when is_record(Stream_record, stream) ->
	Trans = fun() ->
					case mnesia:last(stream) of
						'$end_of_table' -> Id = 1;
						I -> Id = I + 1
					end,
					mnesia:write(Stream_record#stream{id=Id})
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).



%% @doc
%% Function: get_all_streams/0
%% Purpose: Read all streams from the database.
%% Returns: {aborted, Reason} | {error, Reason} | [Record] (#stream)
%%
%% @end
-spec get_all_streams() -> {aborted, term()} | {error, term()} | [record].
get_all_streams() ->
	Trans = fun() ->
					case mnesia:match_object(#stream{_='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "No Such ID"};
						S -> S
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).

-spec get_all_users() -> {aborted, term()} | {error, term()} | [record].
get_all_users() ->
	Trans = fun() ->
					case mnesia:match_object(#user{_='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "No Such ID"};
						S -> S
					end
			end,
	mnesia:wait_for_tables([user], 5000),
	mnesia:activity(transaction, Trans).


%% @doc
%% Function: get_stream_by_id/1
%% Purpose: Read a stream from the database.
%% Returns: {aborted, Reason} | {error, Reason} | Record (#stream)
%%
%% @end
-spec get_stream_by_id(integer()) -> {aborted, term()} |
									   {error, term()} | 
									   record.
get_stream_by_id(Id) when is_integer(Id) ->
	Trans = fun() ->
					case mnesia:match_object(#stream{id=Id, _='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "No Such ID"};
						[S|_] -> S
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).




%% @doc
%% Function: update_stream/1
%% Purpose: Update a stream in the database.
%% Returns: {aborted, Reason} | {error, Reason} | ok
%%
%% Side effects: Updates values of an existing stream, with the same id
%%               as the id in the provided stream, in the database that
%%               are not undefined in the provided stream.
%% @end
-spec update_stream(integer(), record()) -> {aborted, term()} | {error, term()} | ok.
update_stream(Id, Stream) when is_record(Stream, stream) ->
	case get_stream_by_id(Id) of
		{aborted, Reason} -> {aborted, Reason};
		{error, Reason} -> {error, Reason};
		Record ->
			Updated_record =
				Record#stream{
							   type =
								   case Stream#stream.type of
									   undefined -> Record#stream.type;
									   Type -> Type
								   end,
							   latitude =
								   case Stream#stream.latitude of
									   undefined ->
										   Record#stream.latitude;
									   Lat -> Lat
								   end,
							   longitude =
								   case Stream#stream.longitude of
									   undefined ->
										   Record#stream.longitude;
									   Long -> Long
								   end,
							   description =
								   case Stream#stream.description of
									   undefined ->
										   Record#stream.description;
									   Desc -> Desc
								   end,
							   public_access =
								   case Stream#stream.public_access of
									   undefined ->
										   Record#stream.public_access;
									   P_A -> P_A
								   end,
							   public_search =
								   case Stream#stream.public_search of
									   undefined ->
										   Record#stream.public_search;
									   P_S -> P_S
								   end,
							   frozen =
								   case Stream#stream.frozen of
									   undefined -> Record#stream.frozen;
									   Frozen -> Frozen
								   end,
							   history_size =
								   case Stream#stream.history_size of
									   undefined ->
										   Record#stream.history_size;
									   H_S -> H_S
								   end,
							   last_updated =
								   case Stream#stream.last_updated of
									   undefined ->
										   Record#stream.last_updated;
									   L_U -> L_U
								   end,
							   secret_key =
								   case Stream#stream.secret_key of
									   undefined -> Record#stream.secret_key;
									   S_K -> S_K
								   end,
							   owner_id =
								   case Stream#stream.owner_id of
									   undefined -> Record#stream.owner_id;
									   Owner_ID -> Owner_ID
								   end,
							   resource_id =
								   case Stream#stream.resource_id of
									   undefined -> Record#stream.resource_id;
									   Res_ID -> Res_ID
								   end,
							   version =
								   case Stream#stream.version of
									   undefined -> Record#stream.version;
									   Version -> Version
								   end
							  },
			Trans = fun() ->
							mnesia:write(Updated_record)
					end,
			mnesia:wait_for_tables([stream], 5000),
			mnesia:activity(transaction, Trans)
	end.


%% @doc
%% Function: update_user/1
%% Purpose: Update a user in the database.
%% Returns: {aborted, Reason} | {error, Reason} | ok
%%
%% Side effects: Updates values of an existing user, with the same id
%%               as the id in the provided stream, in the database that
%%               are not undefined in the provided user.
%% @end
-spec update_user(integer(), record()) -> {aborted, term()} | {error, term()} | ok.
update_user(Id, User) when is_record(User, user) ->
	case get_user_by_id(Id) of
		{aborted, Reason} -> {aborted, Reason};
		{error, Reason} -> {error, Reason};
		Record ->
			Updated_record =
				Record#user{
				   email =
					   case User#user.email of
						   undefined -> Record#user.email;
						   Type -> Type
					   end,
				   user_name =
					   case User#user.user_name of
						   undefined ->
							   Record#user.user_name;
						   Lat -> Lat
					   end,
				   password =
					   case User#user.password of
						   undefined ->
							   Record#user.password;
						   Long -> Long
					   end,
				   first_name =
					   case User#user.first_name of
						   undefined ->
							   Record#user.first_name;
						   Desc -> Desc
					   end,
				   last_name =
					   case User#user.last_name of
						   undefined ->
							   Record#user.last_name;
						   P_A -> P_A
					   end,
				   description =
					   case User#user.description of
						   undefined ->
							   Record#user.description;
						   P_S -> P_S
					   end,
				   latitude =
					   case User#user.latitude of
						   undefined -> Record#user.latitude;
						   Frozen -> Frozen
					   end,
				   longitude =
					   case User#user.longitude of
						   undefined ->
							   Record#user.longitude;
						   H_S -> H_S
					   end,
				   creation_date =
					   case User#user.creation_date of
						   undefined ->
							   Record#user.creation_date;
						   L_U -> L_U
					   end,
				   last_login =
					   case User#user.last_login of
						   undefined -> Record#user.last_login;
						   S_K -> S_K
					   end							 
					},
			Trans = fun() ->
				mnesia:write(Updated_record)
			end,
			mnesia:wait_for_tables([user], 5000),
			mnesia:activity(transaction, Trans)
	end.



%% @doc
%% Function: delete_stream_with_id/1
%% Purpose: Delete a stream from the database.
%% Returns: ok | {error, Reason} | {aborted, Reason}
%%
%% Side effect: Deletes a row from the database in the table 'stream'
%%              that have id equal to the provided id.
%% @end
-spec delete_stream_with_id(integer()) -> 
		  ok | {error, term()} | {aborted, term()}.
delete_stream_with_id(Id) when is_integer(Id) ->
	Trans = fun() ->
					case mnesia:delete({stream, Id}) of
						{aborted, Reason} -> {aborted, Reason};
						ok -> ok
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).
										 



%% @doc
%% Function: print_stream/0
%% Purpose: Print a stream from the database to stdout.
%% Returns: ok
%%
%% Side effect: Writes to stdout
%% @end
-spec print_stream(record) -> ok.
print_stream(#stream{id=Id, type=Type, latitude=Lat, longitude=Long,
					 description=Desc, public_access=P_A,
					 public_search=P_S, frozen=F, history_size=H_S,
					 last_updated=L_U, secret_key=S_K, owner_id=O_Id,
					 resource_id=R_Id, version=Ver}) ->
	io:format("The stream:~nID: ~w~nType: ~w~nLat: ~w~nLong: ~w~n"
			 ++ "Desc: ~s~nPublic_access: ~w~nPublic_search: ~w~n"
			 ++ "Frozen: ~w~nHist_size: ~w~nLast_up: ~w~n"
			 ++ "Secret_key: ~w~nOwner_id: ~w~nResource_id: ~w~n"
			 ++ "Version: ~w~n", [Id, Type, Lat, Long, Desc, P_A, P_S, F, H_S,
								  L_U, S_K, O_Id, R_Id, Ver]).





%% @doc
%% Function: clear_stream_table/0
%% Purpose: Clear all entries in the 'stream' table.
%% Returns: {atomic, ok} | {aborted, Reason}
%%
%% Side effect: Clears all entries in the 'stream' database table.
%% @end
-spec clear_stream_table() -> {atomic,ok} | {aborted, term()}.
clear_stream_table() ->
	mnesia:wait_for_tables([stream], 5000),
	mnesia:clear_table(stream).


