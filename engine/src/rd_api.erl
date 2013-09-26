%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == rd_api ==
%% This module contains the functionalty to add and
%% update resources in the database
%%
%% @end
-module(rd_api).
-include_lib("stdlib/include/qlc.hrl").
-include("include/database.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([write_resource/1]).

%% @doc
%% Function: write_resource/1
%% Purpose: used to add a new resource or update an exsisting one
%% Returns: {atmoic,ok} | or {term(), term()}
%%
%% Side effects: Remotely writes the database at node given by get_local_db_node()
%% @end
-spec write_resource(Resource::record()) -> {atomic, ok} | {term(), term()}.
write_resource(Resource) ->
	db_api:write_resource(Resource).

%% ====================================================================
%% Internal functions
%% ====================================================================


