%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_help ==
%% This module contains helper functions needed for the polling system 
%%
%% @end

-module(poll_help).

-include("common.hrl").
-include_lib("erlastic_search.hrl").

-export([get_resources_using_polling/0]).




%% ====================================================================
%% API functions
%% ====================================================================


%% @doc
%% Function: get_resources_using_polling/0
%% Purpose: Retrieves all resources from Elastic Search that are using polling.
%% Returns: [] | [Resource, ... ] | {error, Reason}.
%% @end
-spec get_resources_using_polling() -> [] | [string()] | {error, term()}.
get_resources_using_polling() ->
	%% TODO : Post a query to ES to retrieve resources that have a polling-URL.

	JsonQuery = "{\"query\" : {\"filtered\" : " ++
					"{ \"filter\" : {\"exists\" : {\"field\" : \"uri\"}}}}}",
	
	case erlastic_search:search_json(#erls_params{},
									 ?ES_INDEX,
									 "resource",
									 JsonQuery) of
		{error, Reason} -> {error, Reason};
		{ok, Result} ->
			lib_json:get_field(Result, "hits.hits")
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================


