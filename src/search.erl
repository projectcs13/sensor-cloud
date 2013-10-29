%% @author Andreas, Jose
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /users

-module(search).
-export([init/1, 
                allowed_methods/2,
                content_types_accepted/2,
                content_types_provided/2,
                 process_post/2]).

-include("webmachine.hrl").
-include_lib("erlastic_search.hrl").


-define(INDEX, "sensorcloud").


%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
        {ok, undefined}.



%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
        case api_help:parse_path(wrq:path(ReqData)) of                
                [{"_search"}] ->
                        {['POST','GET'], ReqData, State};
                [error] ->
                        {['POST','GET'], ReqData, State}
        end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client. 
%%          A code 406 is returned to the client if we cannot return the media-type that the user has requested. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_provided(ReqData, State) ->
        {[{"application/json", get_search}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%%          A code 406 is returned to the client if we don't accept a media type that the client has sent. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
        {[{"application/json", process_post}], ReqData, State}.


%% @doc
%% Function: delete_resource/2
%% Purpose: Works but need to fix transformation of the return value
%% Returns:  {JSON-object(string), ReqData, State}
%%
%% Side effects: Deletes the User for the database
%% @end
-spec delete_resource(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
delete_resource(ReqData, State) ->
                        {{halt,405}, ReqData, State}.



%% @doc
%% Function: process_post/2
%% Purpose: decodes a JSON object and either adds the new User in the DB or
%% performs search in the User database.
%% It is run automatically for POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts a new User in the database (when for insertion)
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
        process_search_post(ReqData,State).


%% @doc
%% Function: get_search/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects. 
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end
-spec get_search(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_search(ReqData, State) ->
        % URI Search
        {{halt, 200}, ReqData, State}.


%% @doc
%% Function: process_search_post/2
%% Purpose: Used to handle search requests that come from POST requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_post(ReqData, State) ->
        erlang:display("search with json request"),
        {Json,_,_} = api_help:json_handler(ReqData,State),
        FilteredJson = filter_json(Json),
        erlang:display(FilteredJson),
        case erlastic_search:search_json(#erls_params{},?INDEX, "stream", FilteredJson) of % Maybe wanna take more
                {error,Reason1} ->
                        StreamSearch = "error",
                        {{halt,Reason1}, ReqData, State};
                {ok,List1} ->
                        StreamSearch = lib_json:encode(List1) % May need to convert
        end,
        case erlastic_search:search_json(#erls_params{},?INDEX, "user", FilteredJson) of % Maybe wanna take more
                {error,Reason2} ->
                        UserSearch = "error",
                        {{halt,Reason2}, ReqData, State};
                {ok,List2} -> UserSearch = lib_json:encode(List2) % May need to convert
         end,
        SearchResults = "{\"streams\":"++ StreamSearch ++", \"users\":"++ UserSearch ++"}",
        {true,wrq:set_resp_body(SearchResults,ReqData),State}.
%% GROUPS ARE NOT IMPLEMENTED
%%         case erlastic_search:search_json(#erls_params{},?INDEX, "group", FilteredJson) of % Maybe wanna take more
%%                 {error,Reason} -> {{halt,Reason}, ReqData, State};
%%                 {ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State} % May need to convert
%%         end.


%% @doc
%% Function: filter_json/1
%% Purpose: Used to add private filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must\":{\"term\":{\"private\":\"false\"}}}}}}}".

