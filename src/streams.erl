
-module(streams).
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/user.hrl").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> {ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end

allowed_methods(ReqData, State) ->
	case parse_path(wrq:path(ReqData)) of
		[{"streams"}] ->
			{['POST','GET'], ReqData, State};
		[{"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID, "resources", _ResourceID, "streams", "_search"}] ->
		  	{['POST'], ReqData, State};
		[{"users", _UserID, "resources", _ResourceID, "streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID, "resources", _ResourceID, "streams"}] ->
			{['GET','POST'], ReqData, State};
		[{"users", _UserID, "streams", "_search"}] ->
			{['POST'], ReqData, State};
		[{"users", _UserID, "streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID, "streams"}] ->
			{['GET','POST'], ReqData, State};
		[{"streams", "_search"}] ->
			{['POST'], ReqData, State};
		[error] ->
		        {['POST','GET'], ReqData, State}
end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_provided(ReqData, State) ->
	{[{"application/json", get_stream}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_stream}], ReqData, State}.



%% DELETE
delete_resource(ReqData, State) ->
	Id = proplists:get_value('stream', wrq:path_info(ReqData)),
	erlang:display("delete request"),
	erlastic_search:delete_doc("streams","stream", integer_to_list(Id)),
	{true, ReqData, State}.


%% POST
process_post(ReqData, State) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	IsSearch = (string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search"),
	case IsSearch of 
		false ->
			erlang:display("update request"),
			{Stream,_,_} = json_handler(ReqData,State),
			case erlastic_search:index_doc("streams","stream",Stream) of
				{error,Reason} -> {{error,Reason}, ReqData, State};
				{ok,_List} -> {true,ReqData,State}
			end;
		true ->
			erlang:display("search request"),
			URIQuery = wrq:req_qs(ReqData),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserQuery = [];
				UserId ->
					UserQuery = ["q=owner_id:" ++ lists:nth(1,UserId)]
			end,
			case proplists:get_value('res', wrq:path_info(ReqData)) of
				undefined ->
					ResQuery = [];
				ResId ->
					ResQuery = ["q=resource_id:" ++ lists:nth(1,ResId)]
			end,
			SearchQuery = lists:append(URIQuery,lists:append(UserQuery,ResQuery)),
			case erlastic_search:search_limit("streams", "stream", SearchQuery,100) of % Maybe wanna take more
				{error,Reason} -> {{error,Reason}, ReqData, State};
				{ok,List} -> {List,ReqData,State} % May need to convert
			end
			
	end.




put_stream(ReqData, State) ->
	erlang:display("create request"),
	case proplists:get_value('stream', wrq:path_info(ReqData)) of 
		undefined ->
			{Stream,_,_} = json_handler(ReqData, State),
			case erlastic_search:index_doc("streams", "stream", Stream) of	
				{error, Reason} -> {{error, Reason}, ReqData, State};
				{ok,_List} -> {true, ReqData, State}
			end;
		Id ->
			case erlastic_search:search("streams", "stream", ["q=stream_id:" ++ lists:nth(1,Id)]) of
				{error, Reason} -> {{error, Reason}, ReqData, State};
				{ok, []} ->
					{Stream,_,_} = json_handler(ReqData, State),
					case erlastic_search:index_doc_with_id("streams", "stream", lists:nth(1,Id), Stream) of	
						{error, Reason} -> {{error, Reason}, ReqData, State};
						{ok,_List} -> {true, ReqData, State}
					end;
				_ -> {"id already exist",ReqData,State}
			end
	end.







get_stream(ReqData, State) ->
	erlang:display("fetch request"),
	case proplists:get_value('stream', wrq:path_info(ReqData)) of
		undefined ->
		% List streams based on URI
		        erlang:display("Value undefined"),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserQuery = [];
				UserId ->
					UserQuery = ["q=owner_id:" ++ lists:nth(1,UserId)]
			end,
			case proplists:get_value('res', wrq:path_info(ReqData)) of
				undefined ->
					ResQuery = [];
				ResId ->
					ResQuery = ["q=resource_id:" ++ lists:nth(1,ResId)]
			end,
			Query = lists:append(["q=*"],lists:append(UserQuery,ResQuery)),
			case erlastic_search:search_limit("streams", "stream", Query, 100) of % Maybe wanna take more
				{error,Reason} -> {{error, Reason}, ReqData, State};
				{ok,List} -> {List, ReqData, State} % Maybe need to convert
			end;
		StreamId ->
		        erlang:display("Value defined"),
		% Get specific stream
			case erlastic_search:search("streams", "stream", ["q=stream_id:" ++ StreamId])) of 
				{error, Msg} -> erlang:display("got error"),
						{Msg, ReqData, State};
				{ok,List} -> erlang:display("got value"),
					     {List, ReqData, State}
			end
	end.





json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	{struct, JsonData} = mochijson2:decode(Value),
	{JsonData, ReqData, State}.


%% @doc
%% Function: merge_lists/2
%% Purpose: helper function to user_to_json/1, given a list of keys and a list of values, this function
%% will create a list [{Key, Value}], if a value is undefined, it will remove the value and the key
%% that it corresponds, both lists are assumed to be of equal length.
%% Returns: [{Key, Value}] | []
%% @end

%% PRE-COND: Assumes that both lists are of equal size.
merge_lists([], []) -> [];
merge_lists([H|T], [A|B]) ->
	case A of
		undefined -> merge_lists(T,B);
		_ -> [{H,A}]++merge_lists(T,B)
	end.


%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the if from the path.
%% Returns: Id
%% @end
id_from_path(RD) ->
    case wrq:path_info(id, RD) of
        undefined->
            ["users", Id] = string:tokens(wrq:disp_path(RD), "/"),
            Id;
        Id -> Id
    end.


parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	case string:to_integer(B) of
		{V, []} -> [{A,V}|pair(T)];
		{error, no_integer} -> [error]
	end.


%% To-do : HTTP Caching support w etags / header expiration.

