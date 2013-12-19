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
		 process_post/2,
		 get_search/2
		]).

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
                [{"_history"}] ->
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
%% Function: get_search/2
%% Purpose: Handles _history requests. With an input list of stream_ids seperated by ','
%% it will return a proper json object that contains a list of jsonobjects that contain
%% a stream_id and the last 'size' number of datapoints for that specific stream.
%%
%% It is run automatically for GET requests
%%
%% Returns: {Body, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts a new User in the database (when for insertion)
%% @end
-spec get_search(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
get_search(ReqData, State) ->
    case api_help:is_search(ReqData) of
        false -> 
            case wrq:get_qs_value("size",ReqData) of 
                    undefined ->
                        NrValues = 25;
                    Values ->
                        {NrValues, _} = string:to_integer(Values)
            end,
            case wrq:get_qs_value("stream_id",ReqData) of 
                    undefined ->
                        ErrorString = api_help:generate_error(<<"Invalid stream_id">>, 405),
                        {{halt, 405}, wrq:set_resp_body(ErrorString, ReqData), State};
                    StreamIds ->
                        IdList = string:tokens(StreamIds, ","),
                        {get_history(IdList, NrValues, "{\"history\":[]}"), ReqData, State}
            end;
        true ->
            {{halt, 501}, wrq:set_resp_body("Please use POST search instead.", ReqData), State}
    end.

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
%% Function: process_search_post/2
%% Purpose: Used to handle search requests that come from POST requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
process_search_post(ReqData, State) ->
    erlang:display("search with json request"),
    case wrq:get_qs_value("size",ReqData) of 
        undefined ->
            Size = "10";
        SizeParam ->
            Size = SizeParam
    end,
	case wrq:get_qs_value("sort",ReqData) of
        undefined ->
            Sort = "user_ranking.average";
        SortParam ->
            Sort = SortParam
    end,
    case wrq:get_qs_value("from",ReqData) of
        undefined ->
            From = "0";
        FromParam ->
            From = FromParam
    end,
    case wrq:get_qs_value("location",ReqData) of
        undefined ->
            LocationParam = false;
        "true" ->
            LocationParam = true;
        _ ->
            LocationParam = false
    end,
    {Json,_,_} = api_help:json_handler(ReqData,State),
    FilteredJson = filter_json(Json, From, Size, Sort, LocationParam),
    case erlastic_search:search_json(#erls_params{},?INDEX, "stream", FilteredJson) of % Maybe wanna take more
            {error, Reason1} ->
                StreamSearch = {error, Reason1};
            {ok,List1} ->
			case lib_json:get_field(FilteredJson, "query.filtered.query.query_string.query") of
				undefined ->
					erlang:display("No query string text, cannot add a suggestion.");
				QueryString ->
                    add_query_suggestion(QueryString)
			end,
			StreamSearch = lib_json:encode(List1) % May need to convert
    end,
    FilteredJson2 = filter_json(Json, From, Size, Sort, false),
    case erlastic_search:search_json(#erls_params{},?INDEX, "virtual_stream", lib_json:rm_field(FilteredJson2, "sort")) of % Maybe wanna take more
            {error, Reason2} ->
                VStreamSearch = {error, Reason2};
            {ok,List2} ->
                VStreamSearch = lib_json:encode(List2) % May need to convert
    end,
    case erlastic_search:search_json(#erls_params{},?INDEX, "user", lib_json:rm_field(FilteredJson2, "sort")) of % Maybe wanna take more
            {error, Reason3} ->
                    UserSearch = {error, Reason3};
            {ok,List3} -> 
    			UserSearch = lib_json:encode(List3) % May need to convert
     end,
	% check search-results for error
    case {StreamSearch, VStreamSearch, UserSearch} of
        {{error, {Body, Code}},_,_} ->
            ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
        {_,{error, {Body2, Code2}},_} ->
            ErrorString = api_help:generate_error(Body2, Code2),
            {{halt, Code2}, wrq:set_resp_body(ErrorString, ReqData), State};
        {_,_,{error, {Body3, Code3}}} ->
            ErrorString = api_help:generate_error(Body3, Code3),
            {{halt, Code3}, wrq:set_resp_body(ErrorString, ReqData), State};
        _ ->
            SearchResults = "{\"streams\":"++ StreamSearch ++",\"vstreams\":"++ VStreamSearch ++",\"users\":"++ UserSearch ++"}",
            {true,wrq:set_resp_body(SearchResults,ReqData),State}
    end.
	   
%% GROUPS ARE NOT IMPLEMENTED
%%         case erlastic_search:search_json(#erls_params{},?INDEX, "group", FilteredJson) of % Maybe wanna take more
%%                 {error,Reason} -> {{halt,Reason}, ReqData, State};
%%                 {ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State} % May need to convert
%%         end.




%% @doc
%% Function: get_history/2
%% Purpose: Gets the NrValues latest datapoints for each streamid that exists in list IdList
%% Returns: JSON string that contains the data and streamid for each streamid in IdList
%% @end
get_history([], _NrValues, Acc) ->
    Acc;
get_history([Head|Rest], NrValues, Acc) ->
    case erlastic_search:search_limit(?INDEX, "datapoint","stream_id:" ++ Head ++ "&sort=timestamp:desc", NrValues) of
        {error,{Code, Body}} ->
                ErrorString = api_help:generate_error(Body, Code),
                IdAndDataJson = "{\"stream_id\":\""++Head++"\",\"data\":{\"error\": \""++ErrorString++"\"}";
        {ok,JsonStruct} ->
                IdAndDataJson = parse_datapoints(lib_json:get_field(JsonStruct, "hits.hits"),"{\"stream_id\":\""++Head++"\",\"data\":[]}")  
    end,
    get_history(Rest, NrValues, lib_json:add_value(Acc,"history",IdAndDataJson)).


%% @doc
%% Function: parse_datapoints/2
%% Purpose: Gets the NrValues latest datapoints for each streamid that exists in list IdList
%% Returns: JSON string that contains the data and streamid for each streamid in IdList
%% @end
parse_datapoints([], FinalJson) ->
    FinalJson;
parse_datapoints([Head|Rest], FinalJson) ->
    Datapoint = lib_json:rm_field(lib_json:get_field(Head, "_source"), "stream_id"),
    parse_datapoints(Rest, lib_json:add_value(FinalJson,"data",Datapoint)).

%% @doc
%% Function: add_query_suggestion/1
%% Purpose: Adds the suggestion to ES if it does not exist, or adds weight to an existing
%% suggestion.
%% Returns: an atom, error, created or updated depending on if the search suggestion was
%% saved, updated or gave an error
%% @end
add_query_suggestion(QueryString) ->
    case erlastic_search:search_json(#erls_params{},?INDEX,"search_query","{\"query\":{\"term\":{\"search_suggest\":\""++ lib_json:to_string(QueryString) ++"\"}}}") of
        {error,_} ->
            error;
        {ok,JsonStruct} ->
            case lib_json:get_field(JsonStruct, "hits.hits[0]._id") of
                undefined ->
                    case erlastic_search:index_doc(?INDEX,"search_query","{\"search_suggest\":{\"input\":[\""++ lib_json:to_string(QueryString) ++"\"],\"weight\":1}}") of
                        {error,_} ->
                            error;
                        {ok,_} ->
                            created
                    end;
                Id ->
                    case api_help:update_doc(?INDEX,"search_query", binary_to_list(Id), "{\"script\" : \"ctx._source.search_suggest.weight += 1\"}",[]) of
                        {error,_} ->
                            error;
                        {ok,_} ->
                            updated
                    end
            end
    end.
    



%% @doc
%% Function: filter_json/1
%% Purpose: Used to add private filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json) ->
		NewJson = string:sub_string(Json,1,string:len(Json)-1),
		"{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must_not\":{\"term\":{\"private\":\"true\"}}}}}}}".


%% @doc
%% Function: filter_json/4
%% Purpose: Used to add private filters to the json query with pagination
%% Returns: JSON string that is updated with filter and the from size parameters
%% @end
filter_json(Json, From, Size, Sort, false) ->
	case lib_json:get_field(Json, "sort") of 
		undefined -> 
			UseSort = "\"" ++ Sort ++ "\"", 
			SortJson = Json;
		SortValue when is_binary(SortValue) -> 
			UseSort = "\"" ++ binary_to_list(SortValue) ++ "\"",
			SortJson = lib_json:rm_field(Json, "sort");
		SortValue -> 
			UseSort = SortValue,
			SortJson = lib_json:rm_field(Json, "sort")
	end,
	NewJson = string:sub_string(SortJson,1,string:len(SortJson)-1),
	"{\"from\" : "++From++
	",\"size\" : "++Size++
	",\"sort\" : " ++UseSort++
	",\"query\" : {\"filtered\" : "++NewJson++
	",\"filter\" : {\"bool\" : {\"must_not\" : {\"term\" : {\"private\" : \"true\"}}}}}}}";
filter_json(Json, From, Size, Sort, true) ->
    case lib_json:get_field(Json, "sort") of 
        undefined -> 
            UseSort = "\"" ++ Sort ++ "\"", 
            SortJson = Json;
        SortValue when is_binary(SortValue) -> 
            UseSort = "\"" ++ binary_to_list(SortValue) ++ "\"",
            SortJson = lib_json:rm_field(Json, "sort");
        SortValue -> 
            UseSort = SortValue,
            SortJson = lib_json:rm_field(Json, "sort")
    end,
    NewJson = string:sub_string(SortJson,1,string:len(SortJson)-1),
    "{\"script_fields\":{\"location\":{\"script\":\"doc['location'].value\"}},\"fields\":\"_source\","++
    "\"from\" : "++From++
    ",\"size\" : "++Size++
    ",\"sort\" : " ++UseSort++
    ",\"query\" : {\"filtered\" : "++NewJson++
    ",\"filter\" : {\"bool\" : {\"must_not\" : {\"term\" : {\"private\" : \"true\"}}}}}}}".
