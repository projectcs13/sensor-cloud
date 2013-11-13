%% @author Gabriel Tholsgård
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_help_test ==
%% This module contains tests of the helper functions
%% needed for the polling system. 
%%
%% @end

-module(poll_help_test).

-include("common.hrl").
-include("poller.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("inets/include/mod_auth.hrl").

-export([]).


%% ====================================================================
%% API functions
%% ====================================================================

%% @doc
%% Function: inti_test/0
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
	


%% ====================================================================
%% Internal functions
%% ====================================================================



%% @doc
%% Function: clear_resource_type/0
%% Purpose: Transform a list of resources in json format to a list of resources
%%          in the form of the record #pollerInfo.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
-spec clear_resource_type() ->
		  {ok, term()}
		| {ok, saved_to_file}
		| {error, term()}.
clear_resource_type() ->
	httpc:request(delete, {?ES_ADDR ++ "/resource", []}, [], []).



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
			_ -> ", \"uri\" : \"" ++ Url ++ "\""
		end,
	F = "\"polling_freq\" : " ++ integer_to_list(Freq),
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{" ++ F ++ U ++ N ++ "}"
						},
				  [], []).



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
						 "{\"uri\" : \"" ++ Url ++ "\" }"
						}, [], []);
post_resource(Name, Url) ->
	httpc:request(post, {?ES_ADDR ++ "/resource", [],
						 "application/json",
						 "{\"name\" : \"" ++ Name ++ "\"" ++
							 ", \"uri\" : \"" ++ Url ++ "\" }"
						}, [], []).


%% @doc
%% Function: delete_resource_from_record/1
%% Purpose: Delete the specified resource from the resource type in ES.
%% Returns: {ok, Result} | {ok, saved_to_file} | {error, Reason}.
%% @end
delete_resource_from_record(Record) when is_record(Record, pollerInfo) ->
	Id = Record#pollerInfo.resourceid,
	httpc:request(delete, {?ES_ADDR ++ "/resource/" ++ Id, []}, [], []).



















