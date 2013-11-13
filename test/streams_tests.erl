%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams_tests ==
%% This module contains several tests to test the functionallity
%% in the module streams which is done by calling the webbmachine.
%%
%% @end

-module(streams_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).

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
%% Function: process_search_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%% @end
process_search_post_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\", \"private\" : \"false\"}"}, [], []),
    {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"search\",\"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\", \"private\" : \"true\"}"}, [], []),
    DocId1 = lib_json:get_field(Body1,"_id"),
    DocId2 = lib_json:get_field(Body2,"_id"),
    api_help:refresh(),
    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/streams/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
    {ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, _Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
    {ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, _Body10}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(ResourceId), []}, [], []),
	% Test for the search
    ?assertEqual(true,lib_json:get_field(Body3,"hits.total") >= 1).

%% @doc
%% Function: get_stream_test/0
%% Purpose: Test the get_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates a document in elasticsearch
%% @end
-spec get_stream_test() -> ok | {error, term()}.

get_stream_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : \"0\", \"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\", \"private\" : \"false\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"test\" : \"get\",\"user_id\" : \"0\", \"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\", \"private\" : \"false\"}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test get and search
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {"http://localhost:8000/users/0/resources/" ++ lib_json:to_string(ResourceId) ++ "/streams", []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/_search?user_id=0", []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/streams/_search",[],"application/json", "{\"query\":{\"term\" : { \"test\" : \"get\" }}}"}, [], []),
	% Test get for missing index
	{ok, {{_Version7, 404, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(get, {"http://localhost:8000/streams/1", []}, [], []),
	% Test delete
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	
	{{Year,Month,Day},_} = calendar:local_time(),
	Date = generate_date([Year,Month,Day]),
	% Tests to make sure the correct creation date is added
	?assertEqual(true,lib_json:get_field(Body3,"creation_date") == list_to_binary(Date)),
	?assertEqual(<<"get">>,lib_json:get_field(Body3,"test")),
	?assertEqual(true,lib_json:get_field(Body3,"private") == <<"false">>),
	?assertEqual(true,lib_json:field_value_exists(Body4,"streams[*].test", <<"get">>)),
	?assertEqual(true,lib_json:field_value_exists(Body5,"hits.hits[*]._source.test", <<"get">>)),
	?assertEqual(true,lib_json:get_field(Body5,"hits.total") >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,lib_json:field_value_exists(Body5,"hits.hits[*]._source.test", <<"get">>)),
	?assertEqual(true,lib_json:get_field(Body6,"hits.total") >= 2), % Needed in case unempty elasticsearch
	?assertEqual(true,lib_json:get_field(Body8,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body9,"_id") == DocId2).

%% @doc
%% Function: get_stream_test/0
%% Purpose: Test the put_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and updates them
%% @end
-spec put_stream_test() -> ok | {error, term()}.

put_stream_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\",\n\"private\" : \"true\"\n, \"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/" ++ lib_json:to_string(ResourceId) ++ "/streams", [], "application/json", "{\n\"test\" : \"get\",\n\"private\" : \"true\"\n}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test update
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), [], "application/json", "{\n\"test\" : \"put\",\n\"private\" : \"false\"\n}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	% Test get
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	% Test delete
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, _Body9}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(ResourceId), []}, [], []),
	% Test update on missing doc
	{ok, {{_Version10, 404, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(put, {"http://localhost:8000/streams/1", [], "application/json", "{\n\"test\" : \"put\"\n}"}, [], []),
	?assertEqual(<<"false">>,lib_json:get_field(Body5,"private")),
	?assertEqual(true,lib_json:get_field(Body5,"private") =/= <<"true">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") =/= <<"false">>),
	?assertEqual(true,lib_json:get_field(Body6,"private") == <<"true">>),
	?assertEqual(true,lib_json:get_field(Body5,"test") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body5,"test") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body6,"test") == <<"put">>),
	?assertEqual(true,lib_json:get_field(Body6,"test") =/= <<"get">>),
	?assertEqual(true,lib_json:get_field(Body7,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body8,"_id") == DocId2),
	?assertEqual(true,string:str(Body10,"not found") =/= 0).
%% @doc
%% Function: delete_stream_test/0
%% Purpose: Test the delete_stream function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 2 document in elasticsearch and deletes them
%% @end
-spec delete_stream_test() -> ok | {error, term()}.

delete_stream_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	% Test create
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n, \"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/resources/" ++ lib_json:to_string(ResourceId) ++ "/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	DocId1 = lib_json:get_field(Body1,"_id"),
	DocId2 = lib_json:get_field(Body2,"_id"),
	api_help:refresh(),
	% Test delete
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(ResourceId), []}, [], []),
	api_help:refresh(),
	% Test delete on missing index
	{ok, {{_Version6, 404, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId1), []}, [], []),
	{ok, {{_Version7, 404, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId2), []}, [], []),
	?assertEqual(true,lib_json:get_field(Body3,"_id") == DocId1),
	?assertEqual(true,lib_json:get_field(Body4,"_id") == DocId2).


%% @doc
%% Function: create_doc_without_resource_test/0
%% Purpose: Test that error is given when creating stream without resource
%% Returns: ok | {error, term()}
%%
%% @end
-spec create_doc_without_resource_test() -> ok | {error, term()}.

create_doc_without_resource_test() ->
	{ok, {{_Version1, 500, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
 	{ok, {{_Version2, 500, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users/0/streams", [], "application/json", "{\n\"test\" : \"get\"\n}"}, [], []),
	?assertEqual(true,string:str(Body1,"resource_id_missing") =/= 0),
	?assertEqual(true,string:str(Body2,"resource_id_missing") =/= 0).

%% @doc
%% Function: restricted_fields_create_test/0
%% Purpose: Test that creation with restricted fields are not allowed
%% Returns: ok | {error, term()}
%% @end
-spec restricted_fields_create_test() -> ok | {error, term()}.
restricted_fields_create_test() ->
	{ok, {{_Version1, 409, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"quality\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version2, 409, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"user_ranking\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version3, 409, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"subscribers\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version4, 409, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"last_update\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version5, 409, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"creation_date\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	{ok, {{_Version6, 409, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"history_size\" : \"\"\n, \"resource_id\" : \"asdascvsr213sda\"}"}, [], []),
	?assertEqual(true,string:str(Body1,"error") =/= 0),
	?assertEqual(true,string:str(Body2,"error") =/= 0),
	?assertEqual(true,string:str(Body3,"error") =/= 0),
	?assertEqual(true,string:str(Body4,"error") =/= 0),
	?assertEqual(true,string:str(Body5,"error") =/= 0),
	?assertEqual(true,string:str(Body6,"error") =/= 0).
		
		
		
%% @doc
%% Function: restricted_fields_update_test/0
%% Purpose: Test that update with restricted fields are not allowed
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 1 document in elasticsearch and deletes it
%% @end
-spec restricted_fields_update_test() -> ok | {error, term()}.
restricted_fields_update_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\n\"test\" : \"restricted\"\n, \"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 409, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"user_ranking\" : \"\"}"}, [], []),
	{ok, {{_Version3, 409, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"subscribers\" : \"\"}"}, [], []),
	{ok, {{_Version4, 409, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"last_update\" : \"\"}"}, [], []),
	{ok, {{_Version5, 409, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"creation_date\" : \"\"}"}, [], []),
	{ok, {{_Version6, 409, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"history_size\" : \"\"\}"}, [], []),
	{ok, {{_Version7, 409, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"resource_id\" : \"\"\}"}, [], []),
	{ok, {{_Version8, 409, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"type\" : \"\"\}"}, [], []),
	{ok, {{_Version9, 409, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"accuracy\" : \"\"\}"}, [], []),
	{ok, {{_Version10, 409, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"quality\" : \"\"\}"}, [], []),
	{ok, {{_Version11, 409, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"min_val\" : \"\"\}"}, [], []),
	{ok, {{_Version12, 409, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"max_val\" : \"\"\}"}, [], []),
	{ok, {{_Version13, 409, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(put, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), [], "application/json", "{\"location\" : \"\"\}"}, [], []),
	{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, _Body14}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version15, 200, _ReasonPhrase15}, _Headers15, _Body15}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(ResourceId), []}, [], []),
	?assertEqual(true,string:str(Body2,"error") =/= 0),
	?assertEqual(true,string:str(Body3,"error") =/= 0),
	?assertEqual(true,string:str(Body4,"error") =/= 0),
	?assertEqual(true,string:str(Body5,"error") =/= 0),
	?assertEqual(true,string:str(Body6,"error") =/= 0),
	?assertEqual(true,string:str(Body7,"error") =/= 0),
	?assertEqual(true,string:str(Body8,"error") =/= 0),
	?assertEqual(true,string:str(Body9,"error") =/= 0),
	?assertEqual(true,string:str(Body10,"error") =/= 0),
	?assertEqual(true,string:str(Body11,"error") =/= 0),
	?assertEqual(true,string:str(Body12,"error") =/= 0),
	?assertEqual(true,string:str(Body13,"error") =/= 0).		
	
%% @doc
%% Function: server_side_creation_test/0
%% Purpose: Test that the correct fields are added server side
%% Returns: ok | {error, term()}
%%
%% Side effects: creates 1 document in elasticsearch and deletes it
%% @end
-spec server_side_creation_test() -> ok | {error, term()}.

server_side_creation_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"test\" : \"search\"}"}, [], []),
	ResourceId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/streams", [], "application/json", "{\"resource_id\" : \"" ++ lib_json:to_string(ResourceId) ++ "\"}"}, [], []),
	DocId = lib_json:get_field(Body1,"_id"),
	api_help:refresh(),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(delete, {"http://localhost:8000/streams/" ++ lib_json:to_string(DocId), []}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(ResourceId), []}, [], []),
	?assertEqual(true,lib_json:get_field(Body2,"quality") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"user_ranking") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"subscribers") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"last_update") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"creation_date") =/= undefined),
	?assertEqual(true,lib_json:get_field(Body2,"history_size") =/= undefined).


%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_date(DateList::list()) -> string().

generate_date([First]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First);
		false -> "" ++ integer_to_list(First)
	end;
generate_date([First|Rest]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest);
		false -> "" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest)
	end.
