%% @author Gabriel Tholsgård, Li Hao
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
-include("parser.hrl").

-export([get_resources_using_polling/0,
		 json_to_record_resources/1,
		 json_to_record_resource/1,
		 get_parsers_by_id/1,
		 post_datapoint/2,
		 get_datapoint/1]).




%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: get_datapoint/1
%% Purpose: get a datapoint from elasticsearch 
%% Returns: [string() ...] | {error, ErrMsg}
%% @end
-spec get_datapoint(integer() | string()) -> tuple() | list().
get_datapoint(StreamId)->
	case is_integer(StreamId) of
		true->
			Id = integer_to_list(StreamId);
		_->
			Id = StreamId
	end,
	case erlastic_search:search_limit(?ES_INDEX, "datapoint", "streamid:" ++ Id, 100) of
		{ok, Result} ->
			FinalJson = lib_json:get_list_and_add_id(Result, data),
			lib_json:get_field(FinalJson, "data");
		{error, {Code, Body}} -> 
    		ErrorString = api_help:generate_error(Body, Code),
    		erlang:display(ErrorString),
			{error, ErrorString}
	end.

%% @doc
%% Function: post_datapoint/2
%% Purpose: post a new datapoint into the elasticsearch 
%% Returns: ok | {error, ErrMsg}
%% @end
-spec post_datapoint(integer()|string(), any()) -> atom() | tuple().
post_datapoint(StreamId, Value)->
	case is_integer(StreamId) of
		true->
			FieldValue1 = {"streamid",integer_to_binary(StreamId)};
		_ ->
			FieldValue1 = {"streamid",list_to_binary(StreamId)}
	end,
	case is_integer(Value) of
		true->
			FieldValue2 = {"value",integer_to_binary(Value)};
		_->
			case is_float(Value) of
				true->
					FieldValue2 = {"value",float_to_binary(Value)};
				_ ->
					FieldValue2 = {"value",list_to_binary(Value)}
			end
	end,
	{{Year, Month, Day}, {Hour, Minutes, Seconds}} = calendar:now_to_universal_time(os:timestamp()),
	
	StrYear = integer_to_list(Year),
	StrMonth = integer_to_list(Month),
	StrDay = integer_to_list(Day),
	StrHour = integer_to_list(Hour),
	StrMinutes = integer_to_list(Minutes),
	StrSeconds = integer_to_list(Seconds),
	
	FieldValue3 = {"timestamp",list_to_binary(StrYear++":"++StrMonth++":"++StrDay++" "++StrHour++":"++StrMinutes++":"++StrSeconds)},
	FieldValues = [FieldValue1, FieldValue2, FieldValue3],
	FinalJson = lib_json:add_values("{}", FieldValues),
	
	case erlastic_search:index_doc(?ES_INDEX, "datapoint", FinalJson) of
		{error, Reason} -> erlang:display("Failed to insert the new datapoint into the elasticsearch for this reason: "++Reason),
						   {error, Reason};
		{ok,List} -> 
			ok
	end.

%% @doc
%% Function: get_parsers_by_id/1
%% Purpose: get parsers list according to specific resource id 
%% Returns: {error, ErrMsg} | [#parser .....]
%% @end
-spec get_parsers_by_id(string()) -> tuple() | list().
get_parsers_by_id(ResourceId)->
	case erlastic_search:search_limit(?ES_INDEX, "parser", "resource_id:" ++ ResourceId, 100) of
		{ok, Result} ->
			EncodedResult = lib_json:encode(Result),
			case re:run(EncodedResult, "\"max_score\":null", [{capture, first, list}]) of
				{match, _} -> {error, "parsers not found"};
				nomatch -> FinalJsonList = lib_json:get_field(lib_json:get_list_and_add_id(Result), "hits"),
						   
						   %%transform this json list to record list
						   jsonToRecord(FinalJsonList, [])
			end;
		_ ->
			erlang:display("an error happens: parsers not found"),
			{error, "parsers not found!!"}
	end. 

%% @doc
%% Function: get_resources_using_polling/0
%% Purpose: Retrieves all resources from Elastic Search that are using polling.
%% Returns: [] | [Resource, ... ] | {error, Reason}.
%% @end
-spec get_resources_using_polling() -> [] | [string()] | {error, term()}.
get_resources_using_polling() ->
	%% TODO : Post a query to ES to retrieve resources that have a polling-URL.

	JsonQuery = "{\"query\" : {\"filtered\" : " ++
					"{ \"filter\" : {\"exists\" : {\"field\" : \"url\"}}}}}",
	
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
%% Returns: Resource
%% @end
-spec json_to_record_resource(Resource::json()) -> record().
json_to_record_resource(Resource) ->
	Name = case lib_json:get_field(Resource, "_source.name") of
			   undefined -> undefined;
			   N -> binary_to_list(N)
		   end,
	Url = case lib_json:get_field(Resource, "_source.url") of
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


%% @doc
%% Function: jsonToRecord/2
%% Purpose: transform the json list to parser record list 
%% Returns: list()
%% @end
-spec jsonToRecord(list(), list()) -> list().
jsonToRecord([], Res)->
	Res;
jsonToRecord([Item|Tail], Res)->
	Tmp = #parser{resource_id = lib_json:get_field(Item, "resource_id"),
				  stream_id = lib_json:get_field(Item, "stream_id"),
				  input_parser = binary_to_list(lib_json:get_field(Item, "input_parser")),
				  input_type = binary_to_list(lib_json:get_field(Item, "input_type"))
				 },
	jsonToRecord(Tail, [Tmp|Res]).
	

















