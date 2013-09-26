%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == resource_directory_api_tests ==
%% This module contains several tests to test the functionallity
%% in the module resource_directory_api.
%%
%% @end

-module(rd_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("include/database.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: write_resource/0
%% Purpose: Test to add and update resources
%% Returns: ok | {error, term()}
%%
%% Side effects: Writes data to the database
%% @end
-spec write_resource_test() -> ok | {error, term()}.

write_resource_test() ->
% Createing resoruces, all undefined values will have the value undefined
    Resource1 = #resource{id = 1,
						  owner_id = 1},
    Resource2 = #resource{id = 2,
						  owner_id = 2},
    Resource3 = #resource{id = 3,
						  owner_id = 3,
						  version = 1.0},
% Resource updated with new owner
	Resource4 = #resource{id = 3,
						  owner_id = 2},
	rd_api:write_resource(Resource1),
	rd_api:write_resource(Resource2),
	rd_api:write_resource(Resource3),

	{_,Result1} = find_owner(1),
    {_,Result2} = find_owner(2),
    {_,Result3} = find_owner(3),
	{_,Version1} = find_version(3),
	rd_api:write_resource(Resource4),	
	{_,Version2} = find_version(3),	
    {_,Result4} = find_owner(3),


    ?assert(Result1 == [1]),
    ?assert(Result2 == [2]),
    ?assert(Result3 == [3]),
    ?assert(Result4 == [2]),
	?assert(Version1 == [1.0]),
	?assert(Version2 == [1.0]).

%% @doc
%% Function: find_owner/1
%% Purpose: Query the database for the owner of resource with given id
%% Returns: {atomic,list()} | {aborted, Reason}
%%
%% Side effects: Establish a connection to a
%% @end
-spec find_owner(ResourceID::integer()) -> {atomic,list()} | {aborted, atom()}.

find_owner(ResourceID) ->
    F = fun() ->
			Resource = #resource{id = ResourceID,owner_id = '$1', _ = '_'},
			mnesia:select(resource, [{Resource, [], ['$1']}])
        end,
    rpc:call(get_local_db_node(),mnesia,transaction,[F]).

%% @doc
%% Function: find_version/1
%% Purpose: Query the database for the version of resource with given id
%% Returns: {atomic,list()} | {aborted, Reason}
%%
%% Side effects: Query the mnesia database
%% @end
-spec find_version(ResourceID::integer()) -> {atomic,list()} | {aborted, atom()}.

find_version(ResourceID) ->
	F = fun() ->
		Resource = #resource{id = ResourceID,version = '$1', _ = '_'},
		mnesia:select(resource, [{Resource, [], ['$1']}])
    end,
    rpc:call(get_local_db_node(),mnesia,transaction,[F]).

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
