%% @author Kristian Ionescu
%% [www.csproj13.student.it.uu.se]
%% @copyright [none]
%%

-module(restmachine_resource).
-export([init/1, 
	 allowed_methods/2,
	 content_types_accepted/2,
	 content_types_provided/2,
	 process_post/2,
	 delete_resource/2,
	 json_handler/2,
	 json_get/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/database.hrl").

init([]) -> 
	{ok, undefined}.

allowed_methods(ReqData, State) ->
		case parse_path(wrq:path(ReqData)) of
			[{"streams",_}] ->
				erlang:display("GET SPECIFIC STREAM"),
				{['GET', 'PUT', 'DELETE'], ReqData, State};
			[{"streams"}] ->
				erlang:display("GET ALL STREAMS"),
				{['POST','GET'], ReqData, State};
			[error] ->
				{{error, "unspecified stream"}, ReqData, State}
		end.

%% Redirecting GET requests to appropriate media type.
content_types_provided(ReqData, State) ->
	{[{"application/json", json_get}], ReqData, State}.

%% Redirecting PUT requests to appropriate media type.
content_types_accepted(ReqData, State) ->
	{[{"application/json", json_handler}], ReqData, State}.

%% POST
process_post(ReqData, State) ->
	erlang:display("Posting request"),
	json_handler(ReqData, State),
	{true, ReqData, State}.

%% DELETE
delete_resource(ReqData, State) ->
	erlang:display("delete request"),
	
	{true, ReqData, State}.

%% PUT
json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	{struct, JsonData} = mochijson2:decode(Value),
	Stream = json_to_stream(JsonData),
	db_api:add_stream(Stream),
	{true, ReqData, State}.

stream_to_json(Record) ->
  [_ | Values] = tuple_to_list(Record),
  %Keys = restmachine_resource:record_info(fields, RecordName).
  Keys = [<<"id">>, <<"type">>, <<"latitude">>, <<"longitude">>, 
		  <<"description">>, <<"public_access">>, <<"public_search">>,
		  <<"frozen">>, <<"history_size">>, <<"last_updated">>,
		  <<"secret_key">>, <<"owner_id">>, <<"resource_id">>, <<"version">>],
  P_list = merge_lists(Keys, Values),
  erlang:display(P_list),
  mochijson2:encode({struct, P_list}).

%% PRE-COND: Assumes that both lists are of equal size.
merge_lists([], []) -> [];
merge_lists([H|T], [A|B]) ->
	case A of
		undefined -> merge_lists(T,B);
		_ -> [{H,A}]++merge_lists(T,B)
	end.

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

%% GET
%% 1. get the record from database
%% 2. turn record into a proplist
%% 3. convert proplist into json binary using mochijson2:encode
%% 4. return the data.
json_get(ReqData, State) ->
	case proplists:get_value('?', wrq:path_info(ReqData)) of
		% Get all streams
		undefined -> 
			List_of_streams = db_api:get_all_streams(),
			Streams = lists:map(fun(X) -> stream_to_json(X) end, List_of_streams),
			{ Streams, ReqData, State};
		% Get specific stream
		X -> 
			Stream = db_api:get_stream_by_id(list_to_integer(X)),
			{ stream_to_json(Stream), ReqData, State}
	end.

%%parse_path(List) -> [{Key::String(), Value::String()}] | [{Error, Err}]
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
	

