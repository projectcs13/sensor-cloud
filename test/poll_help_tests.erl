%% @author Gabriel Tholsgård, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_help_test ==
%% This module contains tests of the helper functions
%% needed for the polling system. 
%%
%% @end

-module(poll_help_tests).

-include("common.hrl").
-include("poller.hrl").
-include("parser.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/mod_auth.hrl").

-export([]).


%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: init_test/0
%% Purpose: Used to start the inets to be able to do HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: Start inets
%% @end
-spec init_test() -> ok | {error, term()}.
init_test() ->
	inets:start().


%% @doc
%% Function: get_resources_using_polling_test/0
%% Purpose: Retrieves all resources from Elastic Search that are using polling.
%% Returns: ok | {error, term()}.
%% @end
-spec get_resources_using_polling_test() -> ok | {error, term()}.
get_resources_using_polling_test() ->
	
	%% Clear the resource type from all documents.
	clear_resource_type(),
	
	%% Test that should return the empty list, since no resources exists.
	?assertMatch([], poll_help:get_resources_using_polling()),
	
	%% Test that should return the epmty list, since
    %% no resources exists that are using polling.
	post_resource("Test Resource1", ""),
	timer:sleep(1000),
	?assertMatch([], poll_help:get_resources_using_polling()),
	
	%% Test that should return a list of length one.
	post_resource("Test Resource2", "127.0.0.1", 10),
	timer:sleep(1000),
	?assertMatch(1, length(poll_help:get_resources_using_polling())),
	
	%% Test that should return a list of length two.
	post_resource("Test Resource3", "127.0.0.2", 20),
	timer:sleep(1000),
	?assertMatch(2, length(poll_help:get_resources_using_polling())),
	
	%% Clear all entered resources.
	clear_resource_type().




%% @doc
%% Function: json_to_record_resource_test/0
%% Purpose: Transform a resources in json format to a resources
%%          in the form of the record #pollerInfo and to check it is
%%          constructed correctly.
%% Returns: ok | {error, term()}.
%% @end
-spec json_to_record_resource_test() -> ok | {error, term()}.
json_to_record_resource_test() ->
	
	%% Clear the resource type from all documents.
	clear_resource_type(),
	
	%% Enter a resources with and without polling.
    post_resource("Test Resource1", ""),
	post_resource("Test Resource2", "127.0.0.1", 10),
	timer:sleep(1000),
	
	%% Test to get the resource as json and transform it to a #pollerInfo.
	ResourceJsonList = poll_help:get_resources_using_polling(),
	ResourceJson = lists:nth(1, ResourceJsonList),
	ResourceRecord = poll_help:json_to_record_resource(ResourceJson),
	?assertEqual(true, is_record(ResourceRecord, pollerInfo)),
	?assertEqual("Test Resource2", ResourceRecord#pollerInfo.name),
	?assertEqual("127.0.0.1", ResourceRecord#pollerInfo.url),
	?assertEqual(10, ResourceRecord#pollerInfo.frequency),
	
	%% Clear entered resources.
	clear_resource_type(),
	
	%% Enter resources that uses polling but have undefined fields.
	post_resource("", "127.0.0.1"), %% undefined: name, frequency
	timer:sleep(1000),
	List1 = poll_help:get_resources_using_polling(),
	Rec1 = poll_help:json_to_record_resource(lists:nth(1, List1)),
	?assertEqual(undefined, Rec1#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec1#pollerInfo.url),
	?assertEqual(undefined, Rec1#pollerInfo.frequency),
	delete_resource_from_record(Rec1),
	
	post_resource("", "127.0.0.1", 10), %% undefined: name
	timer:sleep(1000),
	List2 = poll_help:get_resources_using_polling(),
	Rec2 = poll_help:json_to_record_resource(lists:nth(1, List2)),
	?assertEqual(undefined, Rec2#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec2#pollerInfo.url),
	?assertEqual(10, Rec2#pollerInfo.frequency),
	delete_resource_from_record(Rec2),
	
	post_resource("Test", "127.0.0.1"), %% undefined: frequency
	timer:sleep(1000),
	List3 = poll_help:get_resources_using_polling(),
	Rec3 = poll_help:json_to_record_resource(lists:nth(1, List3)),
	?assertEqual("Test", Rec3#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec3#pollerInfo.url),
	?assertEqual(undefined, Rec3#pollerInfo.frequency),
	delete_resource_from_record(Rec3).
	
	
	





%% @doc
%% Function: json_to_record_resources_test/0
%% Purpose: Transform a list of resources in json format to a list of resources
%%          in the form of #pollerInfo.
%% Returns: ok | {error, term()}.
%% @end
-spec json_to_record_resources_test() -> ok | {error, term()}.
json_to_record_resources_test() ->
	
	%% Clear the resource type from all documents.
	clear_resource_type(),
	
	%% Enter two resources with and one without polling.
    post_resource("Test Resource1", ""),
	post_resource("Test Resource2", "127.0.0.1", 10),
	post_resource("Test Resource3", "127.0.0.2", 20),
	timer:sleep(1000),
	
	%% Test to get the resource list and transform it to a #pollerInfo list.
	ResourceJsonList = poll_help:get_resources_using_polling(),
	ResourceRecordList = poll_help:json_to_record_resources(ResourceJsonList),
	Record1 = lists:nth(1, ResourceRecordList),
	Record2 = lists:nth(2, ResourceRecordList),
	?assertEqual(2, length(ResourceRecordList)),
	?assertEqual(true, is_record(Record1, pollerInfo)),
	?assertEqual(true, is_record(Record2, pollerInfo)),
	case Record1#pollerInfo.name of
		"Test Resource2" ->
			?assertEqual("127.0.0.1", Record1#pollerInfo.url),
			?assertEqual(10, Record1#pollerInfo.frequency),
			?assertEqual("Test Resource3", Record2#pollerInfo.name),
			?assertEqual("127.0.0.2", Record2#pollerInfo.url),
			?assertEqual(20, Record2#pollerInfo.frequency);
		"Test Resource3" ->
			?assertEqual("127.0.0.2", Record1#pollerInfo.url),
			?assertEqual(20, Record1#pollerInfo.frequency),
			?assertEqual("Test Resource2", Record2#pollerInfo.name),
			?assertEqual("127.0.0.1", Record2#pollerInfo.url),
			?assertEqual(10, Record2#pollerInfo.frequency);
		_ ->
			?assert(false)
	end,
	
	%% Clear all entered resources.
	clear_resource_type().

%% @doc
%% Function: get_parsers_by_id_test/0
%% Purpose: test poll_help:get_parsers_by_id_test/1 function
%% Returns: ok | {error, term()}.
%% @end
-spec get_parsers_by_id_test() -> atom() | {error, term()}.
get_parsers_by_id_test() ->
	
	%% Clear all parsers stored in elasticsearch
	clear_parser_type(),
	api_help:refresh(),
	
	post_parser(1, 13, "application/json","streams/temperature/value"),
	api_help:refresh(),
	ParserList = poll_help:get_parsers_by_id("1"),
	Parser = lists:nth(1, ParserList),
	?assertEqual(1, length(ParserList)),
	?assertEqual(1, Parser#parser.resource_id),
	?assertEqual(13, Parser#parser.stream_id),
	?assertEqual("streams/temperature/value", Parser#parser.input_parser),
	?assertEqual("application/json", Parser#parser.input_type),
	
	%% Clear all entered parsers.
	clear_parser_type().

%% @doc
%% Function: get_datapoint_test/0
%% Purpose: test poll_help:get_datapoint_test/1 function
%% Returns: ok | {error, term()}.
%% @end
-spec get_datapoint_test() -> atom() | tuple().
get_datapoint_test()->
	
	%% clear all datapoints stored in elasticsearch
	clear_datapoint_type(),
	api_help:refresh(),
	
	Res0 = poll_help:get_datapoint(12),
	post_datapoint(12, 13),
	api_help:refresh(),
	Res1 = poll_help:get_datapoint(12),
	?assertEqual(0, length(Res0)),
	?assertEqual(1, length(Res1)),
	?assertEqual(12, lib_json:get_field(lists:nth(1, Res1), "streamid")),
	?assertEqual(13, lib_json:get_field(lists:nth(1, Res1), "value")),
	
	%% clear all datapoints stored in elasticsearch
	clear_datapoint_type().

%% @doc
%% Function: post_datapoint_test/0
%% Purpose: test poll_help:post_datapoint_test/2 function
%% Returns: ok | {error, term()}.
%% @end
-spec post_datapoint_test() -> atom() | tuple().
post_datapoint_test()->
	
	%% clear all datapoints stored in elasticsearch
	clear_datapoint_type(),
	api_help:refresh(),
	
	Res0 = poll_help:get_datapoint(11),
	poll_help:post_datapoint(11,101),
	api_help:refresh(),
	Res1 = poll_help:get_datapoint("11"),
	?assertEqual(0, length(Res0)),
	?assertEqual(1, length(Res1)),
	?assertEqual("11", binary_to_list(lib_json:get_field(lists:nth(1, Res1), "streamid"))),
	?assertEqual("101", binary_to_list(lib_json:get_field(lists:nth(1, Res1), "value"))),
	
	%% clear all datapoints stored in elasticsearch
	clear_datapoint_type().

%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: clear_resource_type/0
%% Purpose: Delete all the resource in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_resource_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_resource_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/resource", []}, [], []).

%% @doc
%% Function: clear_datapoint_type/0
%% Purpose: Delete all the datapoints in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_datapoint_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_datapoint_type()->
	httpc:request(delete, {?ES_ADDR ++ "/datapoint", []}, [], []).

%% @doc
%% Function: clear_parser_type/0
%% Purpose: Delete all the parsers in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_parser_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_parser_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/parser", []}, [], []).

%% @doc
%% Function: post_resource/3
%% Purpose: Post a resource using the values provided, if 'Name' or 'Url' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_resource(Name :: string(), Url :: string(), Freq :: integer()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_resource(Name, Url, Freq) when is_integer(Freq) ->
	N = case Name of
			"" -> "";
			_ -> ", \"name\" : \"" ++ Name ++ "\""
		end,
	U = case Url of
			"" -> "";
			_ -> ", \"url\" : \"" ++ Url ++ "\""
		end,
	F = "\"polling_freq\" : " ++ integer_to_list(Freq),
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{" ++ F ++ U ++ N ++ "}"
						},
				  [], []).

%% @doc
%% Function: post_datapoint/3
%% Purpose: Post a datapoint using the values provided.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_datapoint(StreamId :: integer(), Value :: number()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_datapoint(StreamId, Value) when is_integer(StreamId) ->
	SId = "\"streamid\":" ++ integer_to_list(StreamId) ++ "",
	case is_integer(Value) of
		true->
			Va = integer_to_list(Value);
		_ ->
			case is_float(Value) of
				true->
					Va = float_to_list(Value);
				_->
					Va = Value
			end
	end,
	Val = ", \"value\":" ++ Va ++ "",
	{{Year, Month, Day}, {Hour, Minutes, Seconds}} = calendar:now_to_universal_time(os:timestamp()),
	
	StrYear = integer_to_list(Year),
	StrMonth = integer_to_list(Month),
	StrDay = integer_to_list(Day),
	StrHour = integer_to_list(Hour),
	StrMinutes = integer_to_list(Minutes),
	StrSeconds = integer_to_list(Seconds),
	
	Timestamp = ", \"timestamp\":\""++StrYear++":"++StrMonth++":"++StrDay++" "++StrHour++":"++StrMinutes++":"++StrSeconds++"\"",
	
	FinalData = "{"++SId++Val++Timestamp++"}",
	httpc:request(post, {?ES_ADDR++"/datapoint", [], "application/json",
						 FinalData}, 
				  [], []).

%% @doc
%% Function: post_parser/4
%% Purpose: Post a parser using the values provided, if 'InputType' or 'InputParser' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_parser(ResourceId :: integer(), StreamId :: integer(), InputType :: string(), InputParser :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_parser(ResourceId, StreamId, InputType, InputParser) when is_integer(ResourceId), is_integer(StreamId)->
	It = case InputType of
			 "" -> "";
			 _ -> ", \"input_type\":\"" ++ InputType ++ "\""
		 end,
	Ip = case InputParser of
			 "" -> "";
			 _ -> ", \"input_parser\":\"" ++ InputParser ++ "\""
		 end,
	Ri = integer_to_list(ResourceId),
	Si = integer_to_list(StreamId),
	{ok, Res} = httpc:request(post, {?ES_ADDR ++ "/parser", [],
						 "application/json",
						 "{\"resource_id\":"++Ri++", \"stream_id\":"++Si++It++Ip++"}"
						}, [],[]).

%% @doc
%% Function: post_resource/2
%% Purpose: Post a resource using the values provided, if 'Name' or 'Url' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_resource(Name :: string(), Url :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_resource("", "") ->
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{}"
						}, [], []);
post_resource(Name, "") ->
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{\"name\" : \"" ++ Name ++ "\" }"
						}, [], []);
post_resource("", Url) ->
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{\"url\" : \"" ++ Url ++ "\" }"
						}, [], []);
post_resource(Name, Url) ->
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{\"name\" : \"" ++ Name ++ "\"" ++
							 ", \"url\" : \"" ++ Url ++ "\" }"
						}, [], []).


%% @doc
%% Function: delete_resource_from_record/1
%% Purpose: Delete the specified resource from the resource type in ES.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
delete_resource_from_record(Record) when is_record(Record, pollerInfo) ->
	Id = Record#pollerInfo.resourceid,
	httpc:request(delete, {?ES_ADDR ++ "/resource/" ++ Id, []}, [], []).



















