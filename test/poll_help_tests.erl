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
-include("pubsub.hrl").
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
%% Function: get_streams_using_polling_test/0
%% Purpose: Retrieves all streams from Elastic Search that are using polling.
%% Returns: ok | {error, term()}.
%% @end
-spec get_streams_using_polling_test() -> ok | {error, term()}.
get_streams_using_polling_test() ->
	
	%% Clear the stream type from all documents.
	clear_stream_type(),
	
	%% Test that should return the empty list, since no streams exists.
	?assertMatch([], poll_help:get_streams_using_polling()),
	
	%% Test that should return the epmty list, since
    %% no streams exists that are using polling.
	post_stream("Test Stream1", ""),
	api_help:refresh(),	
	?assertMatch([], poll_help:get_streams_using_polling()),
	
	%% Test that should return a list of length one.
	post_stream("Test Stream2", "127.0.0.1", 10, "", ""),
	api_help:refresh(),	
	?assertMatch(1, length(poll_help:get_streams_using_polling())),
	
	%% Test that should return a list of length two.
	post_stream("Test Stream3", "127.0.0.2", 20, "", ""),
	api_help:refresh(),	
	?assertMatch(2, length(poll_help:get_streams_using_polling())),
	
	%% Clear all entered streams.
	clear_stream_type().



%% @doc
%% Function: json_to_record_stream_test/0
%% Purpose: Transform a streams in json format to a streams
%%          in the form of the record #pollerInfo and to check it is
%%          constructed correctly.
%% Returns: ok | {error, term()}.
%% @end
-spec json_to_record_stream_test() -> ok | {error, term()}.
json_to_record_stream_test() ->
	
	%% Clear the stream type from all documents.
	clear_stream_type(),
	
	%% Enter a streams with and without polling.
    post_stream("Test stream1", ""),
	post_stream("Test stream2", "127.0.0.1", 10, "", ""),
	api_help:refresh(),	
	
	%% Test to get the stream as json and transform it to a #pollerInfo.
	StreamJsonList = poll_help:get_streams_using_polling(),
	StreamJson = lists:nth(1, StreamJsonList),
	StreamRecord = poll_help:json_to_record_stream(StreamJson),
	?assertEqual(true, is_record(StreamRecord, pollerInfo)),
	?assertEqual("Test stream2", StreamRecord#pollerInfo.name),
	?assertEqual("127.0.0.1", StreamRecord#pollerInfo.uri),
	?assertEqual(10, StreamRecord#pollerInfo.frequency),
	
	%% Clear entered streams.
	clear_stream_type(),
	
	%% Enter streams that uses polling but have undefined fields.
	post_stream("", "127.0.0.1"), %% undefined: name, frequency
	api_help:refresh(),	
	List1 = poll_help:get_streams_using_polling(),
	Rec1 = poll_help:json_to_record_stream(lists:nth(1, List1)),
	?assertEqual(undefined, Rec1#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec1#pollerInfo.uri),
	?assertEqual(undefined, Rec1#pollerInfo.frequency),
	delete_stream_from_record(Rec1),
	
	post_stream("", "127.0.0.1", 10, "", ""), %% undefined: name
	api_help:refresh(),	
	List2 = poll_help:get_streams_using_polling(),
	Rec2 = poll_help:json_to_record_stream(lists:nth(1, List2)),
	?assertEqual(undefined, Rec2#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec2#pollerInfo.uri),
	?assertEqual(10, Rec2#pollerInfo.frequency),
	delete_stream_from_record(Rec2),
	
	post_stream("Test", "127.0.0.1"), %% undefined: frequency
	api_help:refresh(),	
	List3 = poll_help:get_streams_using_polling(),
	Rec3 = poll_help:json_to_record_stream(lists:nth(1, List3)),
	?assertEqual("Test", Rec3#pollerInfo.name),
	?assertEqual("127.0.0.1", Rec3#pollerInfo.uri),
	?assertEqual(undefined, Rec3#pollerInfo.frequency),
	delete_stream_from_record(Rec3).
	
	
	



%% @doc
%% Function: json_to_record_streams_test/0
%% Purpose: Transform a list of streams in json format to a list of streams
%%          in the form of #pollerInfo.
%% Returns: ok | {error, term()}.
%% @end
-spec json_to_record_streams_test() -> ok | {error, term()}.
json_to_record_streams_test() ->
	
	%% Clear the stream type from all documents.
	clear_stream_type(),
	
	%% Enter two streams with and one without polling.
    post_stream("Test stream1", ""),
	post_stream("Test stream2", "127.0.0.1", 10, "", ""),
	post_stream("Test stream3", "127.0.0.2", 20, "", ""),
	api_help:refresh(),	
	
	%% Test to get the stream list and transform it to a #pollerInfo list.
	StreamJsonList = poll_help:get_streams_using_polling(),
	StreamRecordList = poll_help:json_to_record_streams(StreamJsonList),
	Record1 = lists:nth(1, StreamRecordList),
	Record2 = lists:nth(2, StreamRecordList),
	?assertEqual(2, length(StreamRecordList)),
	?assertEqual(true, is_record(Record1, pollerInfo)),
	?assertEqual(true, is_record(Record2, pollerInfo)),
	case Record1#pollerInfo.name of
		"Test stream2" ->
			?assertEqual("127.0.0.1", Record1#pollerInfo.uri),
			?assertEqual(10, Record1#pollerInfo.frequency),
			?assertEqual("Test stream3", Record2#pollerInfo.name),
			?assertEqual("127.0.0.2", Record2#pollerInfo.uri),
			?assertEqual(20, Record2#pollerInfo.frequency);
		"Test stream3" ->
			?assertEqual("127.0.0.2", Record1#pollerInfo.uri),
			?assertEqual(20, Record1#pollerInfo.frequency),
			?assertEqual("Test stream2", Record2#pollerInfo.name),
			?assertEqual("127.0.0.1", Record2#pollerInfo.uri),
			?assertEqual(10, Record2#pollerInfo.frequency);
		_ ->
			?assert(false)
	end,
	
	%% Clear all entered streams.
	clear_stream_type().


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: clear_stream_type/0
%% Purpose: Delete all the stream in elasticsearch.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_stream_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_stream_type() ->
	httpc:request(delete, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", []}, [], []).

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
	httpc:request(delete, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/datapoint", []}, [], []).

%% @doc
%% Function: post_stream/3
%% Purpose: Post a stream using the values provided, if 'Name' or 'Uri' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_stream(Name :: string(), Url :: string(), Freq :: integer(), DataType :: string(), Parser :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_stream(Name, Uri, Freq, DataType, Parser) when is_integer(Freq) ->
	N = case Name of
			"" -> "";
			_ -> ", \"name\" : \"" ++ Name ++ "\""
		end,
	U = case Uri of
			"" -> "";
			_ -> ", \"uri\" : \"" ++ Uri ++ "\""
		end,
	F = "\"polling_freq\" : " ++ integer_to_list(Freq),
	D = case DataType of
			"" -> "";
			_ -> ", \"data_type\":\"" ++ DataType ++ "\""
		end,
	P = case Parser of
			"" -> "";
			_ -> ", \"parser\":\"" ++ Parser ++ "\""
		end,
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", [],
						 "application/json",
						 "{" ++ F ++ U ++ N ++ D ++ P ++ ", \"polling\":true}"
						},
				  [], []).

%% @doc
%% Function: post_datapoint/3
%% Purpose: Post a datapoint using the values provided.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_datapoint(StreamId :: string(), Value :: number()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_datapoint(StreamId, Value) when is_list(StreamId) ->
	SId = "\"stream_id\":\"" ++ StreamId ++ "\"",
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
	Timestamp = ", \"timestamp\":\""++?TIME_NOW(erlang:localtime())++"\"",
	
	FinalData = "{"++SId++Val++Timestamp++"}",
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud"++"/datapoint", [], "application/json",
						 FinalData}, 
				  [], []).

%% @doc
%% Function: post_stream/2
%% Purpose: Post a stream using the values provided, if 'Name' or 'Uri' is
%%          empty they are ignored.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec post_stream(Name :: string(), Uri :: string()) ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
post_stream("", "") ->
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", [],
						 "application/json",
						 "{}"
						}, [], []);
post_stream(Name, "") ->
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", [],
						 "application/json",
						 "{\"name\" : \"" ++ Name ++ "\" }"
						}, [], []);
post_stream("", Uri) ->
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", [],
						 "application/json",
						 "{\"uri\" : \"" ++ Uri ++ "\", \"polling\":true}"
						}, [], []);
post_stream(Name, Uri) ->
	httpc:request(post, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream", [],
						 "application/json",
						 "{\"name\" : \"" ++ Name ++ "\"" ++
							 ", \"uri\" : \"" ++ Uri ++ "\", \"polling\":true }"
						}, [], []).


%% @doc
%% Function: delete_stream_from_record/1
%% Purpose: Delete the specified stream from the stream type in ES.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec delete_stream_from_record(Record :: record()) -> {ok, term()} | {ok, saved_to_file} | {error, term()}.
delete_stream_from_record(Record) when is_record(Record, pollerInfo) ->
	Id = Record#pollerInfo.stream_id,
	httpc:request(delete, {api_help:get_elastic_search_url()++"/sensorcloud" ++ "/stream/" ++ Id, []}, [], []).



















