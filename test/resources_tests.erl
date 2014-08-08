%% @author Andreas Moreg�rd <andreas.moregard@gmail.com>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%%
%% @doc Tests for the resource module
%%  This module contains tests for the resource module
%%
%% @end

-module(resources_tests).
-include_lib("eunit/include/eunit.hrl").

-define(WEBMACHINE_URL, api_help:get_webmachine_url()).

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
%%
%% Side effects: creates documents in elasticsearch
%% @end
process_search_post_test() ->
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/resources", [],"application/json", "{\"name\" : \"post\"}"}, [], []),
    DocId1 = lib_json:get_field(Body1,"_id"),
    ?assertEqual(true,lib_json:get_field(Body1,"created")),        
    api_help:refresh(),
    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {?WEBMACHINE_URL++"/resources/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
    {ok, {{_Version3, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {?WEBMACHINE_URL++"/resources/_search", [],"application/json", "{\"query\":{\"match_all\":{}}}"}, [], []),
    {ok, {{_Version8, 200, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId1), []}, [], []),
    ?assertEqual(true,lib_json:get_field(Body3,"hits.total") >= 1),
    ?assertEqual(true,lib_json:get_field(Body4,"hits.total") >= 0).   

%% @doc
%% Function: process_post_test/0
%% Purpose: Test the process_post_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates documents in elasticsearch
%% @end
process_post_test() ->
        {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/resources", [],"application/json", "{\"name\" : \"post\"}"}, [], []),
        api_help:refresh(),  
        ?assertEqual(true,lib_json:get_field(Body1,"created")),        
        %Clean up after the test
        httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(lib_json:get_field(Body1,"_id")), []}, [], []).

%% @doc
%% Function: delete_resource_test/0
%% Purpose: Test the delete_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and deletes documents in elasticsearch
%% @end
delete_resource_test() ->
        {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {?WEBMACHINE_URL++"/resources", [],"application/json", "{\"name\" : \"delete\"}"}, [], []),
        DocId = lib_json:get_field(Body2,"_id"),
        api_help:refresh(),
        {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []),
        api_help:refresh(),
        % Delete a resource that doesn't exist
        {ok, {{_Version5, 404, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(delete, {?WEBMACHINE_URL++"/resources/1", []}, [], []),
        ?assertEqual(true, lib_json:get_field(Body2,"created")),
        ?assertEqual(true, lib_json:get_field(Body3,"found")).
        
%% @doc
%% Function: put_resource_test/0
%% Purpose: Test the put_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and updatess a document in elasticsearch
%% @end
put_resource_test() ->
        {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/resources/", [],"application/json", "{\"name\" : \"put1\"}"}, [], []),
        api_help:refresh(),
        DocId = lib_json:get_field(Body1,"_id"),
        {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(put, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId) , [],"application/json", "{\"name\" : \"put2\"}"}, [], []),
        {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []),
        %Try to put to a resource that doesn't exist
        {ok, {{_Version4, 404, _ReasonPhrase4}, _Headers4, _Body4}} = httpc:request(put, {?WEBMACHINE_URL++"/resources/" ++ "non-existing-index", [],"application/json", "{\"name\" : \"put2\"}"}, [], []),
        ?assertEqual(true,lib_json:get_field(Body1,"created")),

erlang:display("BODY1!!!!:: " ++ Body2), 
        ?assertNotEqual(null,lib_json:get_field(Body2,"_id")),
        ?assertEqual(<<"put2">>,lib_json:get_field(Body3,"name")),
        %Clean up
        httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []).

        
%% @doc
%% Function: get_resource_test/0
%% Purpose: Test the get_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and returns documents in elasticsearch
%% @end
get_resource_test() ->
    {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/resources/", [],"application/json", "{\"name\" : \"get\"}"}, [], []),
    api_help:refresh(),
    DocId = lib_json:get_field(Body1,"_id"),
    {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []),
    {ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(get, {?WEBMACHINE_URL++"/resources/_search?name=get", []}, [], []),
    %Get resource that doesn't exist
    {ok, {{_Version5, 404, _ReasonPhrase5}, _Headers5, _Body5}} = httpc:request(get, {?WEBMACHINE_URL++"/resources/1", []}, [], []),
    % Tests to make sure the correct creation date is added
    ?assertEqual(<<"get">>,lib_json:get_field(Body2,"name")),
    ?assertEqual(true , lib_json:get_field(Body4,"hits.total") >= 1),
    %Clean up
    httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []).


%% @doc
%% Function: get_resource_test/0
%% Purpose: Test the get_resource_test function by doing some HTTP requests
%% Returns: ok | {error, term()}
%%
%% Side effects: creates and returns documents in elasticsearch
%% @end
get_streams_suggested_test() ->
    {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/resources/", [],"application/json", "{\"name\" : \"sugg\" , \"model\" : \"model1\", \"manufacturer\" : \"man1\"}"}, [], []),
    {ok, {{_VersionUser, 200, _ReasonPhraseUser}, _HeadersUser, _BodyUser}} = httpc:request(post, {?WEBMACHINE_URL++"/users", [], "application/json", "{\"username\":\"1\"}"}, [], []),
    api_help:refresh(),
    DocId = lib_json:get_field(Body1,"_id"),

    {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"name\" : \"suggStream\",\"type\": \"Temperature\",\"user_id\" : \"1\", \"resource\" : {\"resource_type\" :\""++ lib_json:to_string(DocId) ++ "\", \"uuid\" : \"1\" }}"}, [], []),
      %Get the created resource in order to check the new new suggested stream

    {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(get, {"http://localhost:8000/resources/" ++ lib_json:to_string(DocId), []}, [], []),

    % Tests to make sure the correct creation date is added
    ?assertEqual(<<"suggStream">>,lib_json:get_field(Body2,"streams_suggest[0].name")),

    {ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"name\" : \"suggStr\",\"type\": \"Temperature\",\"user_id\" : \"1\", \"resource\" : {\"resource_type\" :\""++ lib_json:to_string(DocId) ++ "\", \"uuid\" : \"1\" }}"}, [], []),
    {ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(get, {"http://localhost:8000/resources/" ++ lib_json:to_string(DocId), []}, [], []),

    ?assertEqual(undefined,lib_json:get_field(Body5,"streams_suggest[1].name")),


    %Clean up
    httpc:request(delete, {"http://localhost:8000/resources/" ++ lib_json:to_string(DocId), []}, [], []).

%% @doc
%% Function: add_unsupported_field_test/0
%% Purpose: Test that unsuported fields are not allowed to be added 
%%          on create or update
%% Returns: ok | {error, term()}
%% @end
-spec add_unsupported_field_test() -> ok | {error, term()}.
add_unsupported_field_test() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {?WEBMACHINE_URL++"/resources", [], "application/json", "{\"name\" : \"test\"}"}, [], []),
	DocId = lib_json:get_field(Body,"_id"),
	api_help:refresh(),
	{ok, {{_Version1, 403, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {?WEBMACHINE_URL++"/resources", [],"application/json", "{\"test\":\"asdas\",\"name\" : \"test\"}"}, [], []),
	{ok, {{_Version2, 403, _ReasonPhrase2}, _Headers2, _Body2}} = httpc:request(put, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), [],"application/json", "{\"test\":\"asdas\",\"name\" : \"test\"}"}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, _Body3}} = httpc:request(delete, {?WEBMACHINE_URL++"/resources/" ++ lib_json:to_string(DocId), []}, [], []).


%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%% from the input which should be the list
%% [Year,Mounth,Day]
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
