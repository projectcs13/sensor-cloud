%% @author Tholsg�rd Gabriel
%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.1
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
-include("include/unique_ids.hrl").
%% ====================================================================
%% API functions
%% ====================================================================

-export([start/0, connect/2,write_resource/1, traverse/1]).
-export([create_user/2, get_user_by_id/1, get_user_by_username/1, 
		authenticate/2, change_password/3, exists_username/1]).



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
%% Function: start/0
%% Purpose: Starts process node for database connection.
%% Returns: ok | or {error, term()}
%%
%% Side effects: Starts a process for handling a database connection.
%% @end
-spec start() -> ok | {error, term()}.
start() ->
	ok.


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
	F = fun() -> 
		mnesia:read({user, ID}) 
	end,
	case User = mnesia:activity(transaction, F) of
		[] -> {error, unknown_user};
		_ -> User
	end.

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







