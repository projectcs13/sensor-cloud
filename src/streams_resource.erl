%% @author Kristian Ionescu
%% [www.csproj13.student.it.uu.se]
%% @copyright [none]
%%

-module(streams_resource).
-export([init/1, 
	 allowed_methods/2,
 	 allow_missing_post/2,
	 resource_exists/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 process_post/2,
	 delete_resource/2,
	 json_handler/2,
	 json_get/2,
	 put_resource/2,
	 post_is_create/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/stream.hrl").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.

init([]) -> 
	{ok, undefined}.

post_is_create(ReqData, State) -> {false, ReqData, State}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end

allowed_methods(ReqData, State) ->
		case parse_path(wrq:path(ReqData)) of
			[{"streams",_}] ->
				{['GET', 'PUT', 'DELETE'], ReqData, State};
			[{"streams"}] ->
				{['POST','GET'], ReqData, State};
			[error] ->
				{['POST','GET'], ReqData, State}
		end.

%% @doc
%% Function: allow_missing_post/2
%% Purpose: If the resource accepts POST requests to nonexistent resources, then this should return true.
%% Returns: {true, ReqData, State}
%% @end

allow_missing_post(ReqData, State) ->
	{true, ReqData, State}.

%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client. 
%%          A code 406 is returned to the client if we cannot return the media-type that the user has requested. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end

content_types_provided(ReqData, State) ->
	{[{"application/json", json_get}], ReqData, State}.

%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%%          A code 406 is returned to the client if we don't accept a media type that the client has sent. 
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end

content_types_accepted(ReqData, State) ->
	{[{"application/json", put_resource}], ReqData, State}.

%% @doc
%% Function: process_post/2
%% Purpose: Adds a stream to the database on a 'POST' method.
%% Returns: {true, ReqData, State} | {{error, Reason}, ReqData, State}
%% @end

process_post(ReqData, State) ->
	{Stream, _, _} = json_handler(ReqData, State),
	case db_api:add_stream(Stream) of
		{aborted, Reason} -> {{error, Reason}, ReqData, State};
		{error, Reason} -> {{error, Reason}, ReqData, State};
		ok -> {true, ReqData, State}
	end.

%% @doc
%% Function: delete_resource/2
%% Purpose: deletes a stream on the database on a 'DELETE' method by using the id specified in the url, 
%%          if no url is specified a 404 not found is handled by resource_exists/2.
%% Returns: {true, ReqData, State}
%% @end

delete_resource(ReqData, State) ->
	Id = proplists:get_value('?', wrq:path_info(ReqData)),
	db_api:delete_stream_with_id(list_to_integer(Id)),
	{true, ReqData, State}.

%% @doc
%% Function: json_handler/2
%% Purpose: decodes a JSON object and returns a record representation of this.
%% Returns: {Stream :: record, ReqData, State}
%% @end

json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	case Value of
		[] -> {{error, "empty body"}, ReqData, State};
		_ ->
			{struct, JsonData} = mochijson2:decode(Value),
			Stream = json_to_stream(JsonData),
			{Stream, ReqData, State}
	end.

%% @doc
%% Function: stream_to_json/1
%% Purpose: decodes a record 'streams' to a JSON object and returns it.
%% Returns: obj :: JSON()
%% @end

stream_to_json(Record) ->
  [_ | Values] = tuple_to_list(Record),
  %Keys = restmachine_resource:record_info(fields, RecordName).
  Keys = [<<"id">>, <<"type">>, <<"latitude">>, <<"longitude">>, 
		  <<"description">>, <<"public_access">>, <<"public_search">>,
		  <<"frozen">>, <<"history_size">>, <<"last_updated">>,
		  <<"secret_key">>, <<"owner_id">>, <<"resource_id">>, <<"version">>],
  P_list = merge_lists(Keys, Values),
  mochijson2:encode({struct, P_list}).

%% @doc
%% Function: merge_lists/2
%% Purpose: helper function to stream_to_json/1, given a list of keys and a list of values, this function
%%			will create a list [{Key, Value}], if a value is undefined, it will remove the value and the key 
%% 			that it corresponds, both lists are assumed to be of equal length.
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
%% Function: json_to_stream/1
%% Purpose: Given a proplist, the return value will be a 'stream' record with the values taken from the proplist.
%% Returns: stream::record()
%% @end

json_to_stream(JsonData) ->
	#stream{id = proplists:get_value(<<"id">>, JsonData),
		type = proplists:get_value(<<"type">>, JsonData), 
		latitude = proplists:get_value(<<"latitude">>, JsonData),
		longitude = proplists:get_value(<<"longitude">>, JsonData), 
		description = proplists:get_value(<<"description">>, JsonData), 
		public_access = proplists:get_value(<<"public_access">>, JsonData), 
		public_search = proplists:get_value(<<"public_search">>, JsonData),
		frozen = proplists:get_value(<<"frozen">>, JsonData), 
		history_size = proplists:get_value(<<"history_size">>, JsonData), 
		last_updated = proplists:get_value(<<"last_updated">>, JsonData),
		secret_key = proplists:get_value(<<"secret_key">>, JsonData), 
		owner_id = proplists:get_value(<<"owner_id">>, JsonData), 
		resource_id = proplists:get_value(<<"resource_id">>, JsonData), 
		version = proplists:get_value(<<"version">>, JsonData)
	}.

%% @doc
%% Function: resource_exists/2
%% Purpose: Callback function used by webmachine, checks to see if a resource exists and returns true,
%%          returns false otherwise.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end

resource_exists(ReqData, State) ->
	erlang:display("resource_exists"),
    case parse_path(wrq:path(ReqData)) of
        [error] ->
            {false, ReqData, State};
		[{"streams"}] -> 
			case db_api:get_all_streams() of
				{aborted, _} -> {false, ReqData, State};
				{error, _} -> erlang:display("no streams"), {false, ReqData, State}; 
				_ -> {true, ReqData, State}
			end;
        [{"streams", X}] -> 
			case db_api:get_stream_by_id(X) of
				{aborted, _} -> erlang:display("case 1"), {false, ReqData, State};
				{error, _} -> erlang:display("case 2 false"), {false, ReqData, State};
				_ -> erlang:display("case 3"), {true, ReqData, State}
			end	 
    end. 

%% @doc
%% Function: json_get/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects. 
%%  		Fault tolerance is handled by resources_exists/2.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end

json_get(ReqData, State) ->
	case proplists:get_value('?', wrq:path_info(ReqData)) of
		undefined -> 
			% Get all streams
			Streams = lists:map(fun(X) -> stream_to_json(X) end, db_api:get_all_streams()),
			{Streams, ReqData, State};
		X -> 
			% Get specific stream
			Stream = db_api:get_stream_by_id(list_to_integer(X)),
		 	{stream_to_json(Stream), ReqData, State}
	end.

%% @doc
%% Function: put_resource/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects. 
%%  		Fault tolerance is handled by resources_exists/2.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end

put_resource(ReqData, State) ->
	Id = proplists:get_value('?', wrq:path_info(ReqData)),
	erlang:display("put request"),
	{Stream, _,_} = json_handler(ReqData, State),
	case db_api:get_stream_by_id(list_to_integer(Id)) of
		{aborted, Reason} -> {{error, Reason}, ReqData, State};
		{error, Reason} -> {{error, Reason}, ReqData, State};
		_ -> db_api:update_stream(list_to_integer(Id), Stream),
			 {true, ReqData, State}
	end.

%% @doc
%% Function: parse_path/1
%% Purpose: Given a string representation of a search path, the path is split by the '/' token
%%			and the return value is a list of tuples [{dir, id}].
%% Returns: [{"directory_name", "id_value"}] | [{Error, Err}] | []
%% @end

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
%% To-do : Basic authentication.

