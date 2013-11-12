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
-include("erlson.hrl").
-include("json.hrl").
-include("poller.hrl").

-export([get_resources_using_polling/0,
		 json_to_record_resources/1,
		 json_to_record_resource/1]).




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



%% @doc
%% Function: json_to_record_resources/1
%% Purpose: Converts a list of resource Jsons to a list of pollerInfo records
%% Returns: [] | [Resource, ...]
%% @end
-spec json_to_record_resources([json()]) -> [] | [record()].
json_to_record_resources([]) -> [];
json_to_record_resources([H|T]) ->
	[json_to_record_resource(H) | json_to_record_resources(T)].




%% @doc
%% Function: json_to_record_resource/1
%% Purpose: Converts a resource Json to a pollerInfo record
%% Returns: [] | Resource
%% @end
-spec json_to_record_resource(Resource::json()) -> record().
json_to_record_resource(Resource) ->
	Name = case lib_json:get_field(Resource, "_source.name") of
			   undefined -> undefined;
			   N -> binary_to_list(N)
		   end,
	Url = case lib_json:get_field(Resource, "_source.uri") of
			  undefined -> undefined;
			  U -> binary_to_list(U)
		  end,
	#pollerInfo{resourceid = binary_to_list(lib_json:get_field(Resource, "_id")),
				name = Name,
				url = Url,
				frequency = lib_json:get_field(Resource, "_source.polling_freq")}.


%% ====================================================================
%% Internal functions
%% ====================================================================

	

















