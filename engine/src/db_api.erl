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
-include("include/database.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, connect/2,write_resource/1]). 

%% @doc
%% Function: write_resource/1
%% Purpose: used to add a new resource or update an exsisting one
%% Returns: {atmoic,ok} | or {term(), term()}
%%
%% Side effects: Remotely writes the database at node given by get_local_db_node()
%% @end
-spec write_resource(Resource::record()) -> {atomic, ok} | {term(), term()}.
write_resource(Resource) ->
	Fun = fun() ->		
              mnesia:write(Resource)
          end,
	rpc:call(get_local_db_node(),mnesia,transaction,[Fun]).

%% ====================================================================
%% Internal functions
%% ====================================================================


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
















