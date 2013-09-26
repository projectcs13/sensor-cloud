%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   www.csproj13.student.it.uu.se
%% @version 1.0
%% @copyright Copyright information
%% 
%%
%% @doc == database ==
%% Creates a local database running at node database@"hostname" where hostname is the local hostname, will lose all data when shutdown
%%
%% @end
-module(database).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("include/user.hrl").
-include("include/unique_id.hrl").
-include("include/record.hrl").
-include("include/stream.hrl").

%% @doc
%% Function: init/0 
%% Purpose: start up the database, create the tables and run the tests
%% Args: (none)
%% Returns: atom()
%% Side effects: Starts up a local mnesia database at node database@localhost and creates the tables stream, user and resource
%% @end

-spec init() -> atom().

init() ->
    {ok,Host} = inet:gethostname(),
    FullHost = string:concat("database@",Host), 
    HostAtom = list_to_atom(FullHost),

    mnesia:create_schema([HostAtom]),
    mnesia:start(),
    mnesia:create_table(stream,
                        [{attributes, record_info(fields, stream)}]),
    mnesia:create_table(user,
                        [{attributes, record_info(fields, user)}]),
    mnesia:create_table(resource,
                        [{attributes, record_info(fields, resource)}]),
	mnesia:create_table(unique_ids, 
						[{attributes, record_info(fields, unique_ids)}]).


