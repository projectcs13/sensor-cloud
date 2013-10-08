-module(resource).
-compile(export_all).

-include_lib("webmachine.hrl").
-include("user.hrl").

-define(INDEX, "sensorcloud").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
	%erlastic_search_app:start(), %% start this in the make file somehow
    {ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {List of allowed HTTP requests, string , string()}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
	case parse_path(wrq:path(ReqData)) of
		[ {"resource"}] ->
			{['GET','POST'], ReqData, State};
		[ {"resource", _ResourceID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"user", _UserID}, {"resource"}] ->
			{['GET','POST'], ReqData, State};
		[{"user", _UserID}, {"resource", "_search" ++ _Query}] ->
		  	{['GET', 'POST'], ReqData, State};
		[{"user", _UserID}, {"resource", _ResourceID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[error] ->
		    {['POST','GET'], ReqData, State}
end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_provided(ReqData, State) ->
	{[{"application/json", get_resource}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_resource}], ReqData, State}.


%% @doc
%% Function: delete_resource/2
%% Purpose: Works but need to fix transformation of the return value
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec delete_resource(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
delete_resource(ReqData, State) ->
	Id = proplists:get_value('resourceid', wrq:path_info(ReqData)),
	erlang:display("delete request - check permission here"),
	erlang:display(Id),
	case erlastic_search:delete_doc(?INDEX,"resource", Id) of
			{error,Reason} -> {{halt,Reason}, ReqData, State};
			{ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State}
	end.


%% @doc
%% Function: process_post/2
%% Purpose: Handle POST request
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {atom(), tuple(), string()}.
process_post(ReqData, State) ->
	erlang:display("POST"),
	URIList = string:tokens(wrq:path(ReqData), "/"),
	IsSearch = (string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search"),
	case IsSearch of 
		false ->
			erlang:display("create request"),
			{Resource,_,_} = json_handler(ReqData,State),
			case erlastic_search:index_doc(?INDEX,"resource",Resource) of %should update instead
				{error, Reason} -> 
					{{halt,Reason}, ReqData, State};
				{ok,List} ->
					erlang:display("NO ERROR"),
					{ok,wrq:set_resp_body(json_encode(List),ReqData),State}
			end;
		true ->
			erlang:display("search request"),
			erlang:display("~s", wrq:req_body(ReqData)),
			URIQuery = wrq:req_qs(ReqData),
			case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
					Query = [];
				UserId ->
					Query = "owner:" ++ UserId
			end,
			FullQuery = lists:append(transform(URIQuery,true),Query),
			erlang:display(FullQuery),
			case erlastic_search:search_limit(?INDEX, "resource", FullQuery,10) of % Maybe wanna take more
				{error,Reason} -> {{halt,Reason}, ReqData, State};
				{ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State} % May need to convert
			end

	end.



%% @doc
%% Function: put_resource/2
%% Purpose: Handle PUT request
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec put_resource(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
put_resource(ReqData, State) ->
	case proplists:get_value('resourceid', wrq:path_info(ReqData)) of 
		undefined ->
			{"id does not exist",ReqData,State};
		Id ->
			Result = erlastic_search:search(?INDEX, "resource", ["id:" ++ Id]),
			{_Msg,Data} = Result,
			erlang:display(json_encode(Data)),
			case Result of
				{error, Reason} -> {{error, Reason}, ReqData, State};
				{ok, []} ->
					{"id does not exist",ReqData,State};
				_ -> 
					{Resource,_,_} = json_handler(ReqData,State),
					case erlastic_search:update_doc(?INDEX,"resource",Id,Resource) of %should update instead
						{error,Reason} -> {{error,Reason}, ReqData, State};
						{ok,List} -> {json_encode(List),ReqData,State}
					end
			end
	end.






%% @doc
%% Function: get_resource/2
%% Purpose: Handle GET request
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec get_resource(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_resource(ReqData, State) ->
	erlang:display("fetch request"),
	case proplists:get_value('resourceid', wrq:path_info(ReqData)) of
		undefined ->
		% List resources based on URI
		    erlang:display("Value undefined"),
			case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
					Query = [];
				UserId ->
					Query = "owner:" ++ UserId
			end,
			case erlastic_search:search_limit(?INDEX, "resource", Query, 10) of % Maybe wanna take more
				{error,Reason} -> {{halt, Reason}, ReqData, State};
				{ok,List} -> 
					{json_encode(List), ReqData, State} % Maybe need to convert
			end;
		ResourceId ->
		        erlang:display("Value defined"),
		% Get specific resource
			case erlastic_search:get_doc(?INDEX, "resource", ResourceId) of 
				{error,_Msg} -> 
						erlang:display("got error"),
						{{halt, 404}, ReqData, State};
				{ok,List} -> 
						 erlang:display("got value"),
					     {json_encode(List), ReqData, State}
			end
	end.




%% @doc
%% Function: json_handler/2
%% Purpose: Handles JSON (?)
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec json_handler(tuple(), string()) -> {string(), tuple(), string()}.
json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	erlang:display(Value),
	%%{struct, JsonData} = mochijson2:decode(Value),
	{Value, ReqData, State}.


%% @doc
%% Function: merge_lists/2
%% Purpose: helper function to user_to_json/1, given a list of keys and a list of values, this function
%% will create a list [{Key, Value}], if a value is undefined, it will remove the value and the key
%% that it corresponds, both lists are assumed to be of equal length.
%% PRE-COND: Assumes that both lists are of equal size.
%% Returns: [{Key, Value}] | []
%% @end
-spec merge_lists(list(), list()) -> list().
merge_lists([], []) -> [];
merge_lists([H|T], [A|B]) ->
	case A of
		undefined -> merge_lists(T,B);
		_ -> [{H,A}]++merge_lists(T,B)
	end.


%% @doc
%% Function: id_from_path/1
%% Purpose: Retrieves the id from the path.
%% Returns: Id
%% @end
-spec id_from_path(tuple()) -> string().
id_from_path(RD) ->
    case wrq:path_info(id, RD) of
        undefined->
            ["users", Id] = string:tokens(wrq:disp_path(RD), "/"),
            Id;
        Id -> Id
    end.

%% @doc
%% Function: parse_path/1
%% Purpose: Parses the path into a paired list
%% Returns: A list returned by pair()
%% @end
-spec parse_path(string()) -> list().
parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

%% @doc
%% Function: pair
%% Purpose: Pairs the values of the input list
%% Returns: A list with paired values
%% @end
-spec pair(list()) -> list().
pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].

%% @doc
%% Function: transform/2
%% Purpose: Takes the fields and values from the input list and parses them to a string
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec transform(list(),boolean()) -> string().
transform([],true) -> "&";
transform([],false) -> "";
transform([{Field,Value}|Rest],AddAnd) ->
	case Rest of 
		[] -> Field ++ ":" ++ Value ++ transform(Rest,AddAnd);
		_ -> Field ++ ":" ++ Value ++ "&" ++ transform(Rest,AddAnd)
	end.

% Taken from erlasticsearch

%% @doc
%% Function: json_encode/2
%% Purpose: To encode utf8-json WITHOUT converting multi-byte utf8-chars into ASCII '\uXXXX'.
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec json_encode(string()) -> string().
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).

%% To-do : HTTP Caching support w etags / header expiration.


