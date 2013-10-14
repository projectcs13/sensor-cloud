%% @author Jacob Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /datapoints

-module(datapoints_resource).
-export([init/1, allowed_methods/2, content_types_provided/2,
		 		content_types_accepted/2, process_post/2, get_datapoint/2]).

-include("webmachine.hrl").

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
	case parse_path(wrq:path(ReqData)) of	
		[{"datapoints","_search"}] ->
			{['POST','GET'], ReqData, State};
		[{"datapoints",_Id}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"datapoints"}] ->
			{['POST','GET'], ReqData, State};
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
		{[{"application/json", get_datapoint}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
		{[{"application/json", put_datapoint}], ReqData, State}.


%% @doc
%% Function: process_post/2
%% Purpose: decodes a JSON object and either adds the new datapoint in the DB or
%% performs search in the Datapoint database.
%% It is run automatically for POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts a new Datapoint in the database (when for insertion)
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
	erlang:display("posted"),
			{DatapointJson,_,_} = json_handler(ReqData, State),
			case erlastic_search:index_doc(?INDEX, "datapoint", DatapointJson) of
				{error, Reason} -> {{error, Reason}, ReqData, State};
				{ok,_} -> {true, ReqData, State}
			end.


%% @doc
%% Function: json_handler/2
%% Purpose: Handles JSON (?)
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec json_handler(tuple(), string()) -> {string(), tuple(), string()}.
json_handler(ReqData, State) ->
		[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
		{Value, ReqData, State}.


%% @doc
%% Function: get_datapoint/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end
-spec get_datapoint(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_datapoint(ReqData, State) ->
		case is_search(ReqData) of
			false ->
				case id_from_path(ReqData) of
					undefined ->
						% Get all datapoints
						case erlastic_search:search(?INDEX,"datapoint","*:*") of
							{ok, Result} ->
								{json_encode(Result), ReqData, State};
							_ -> {{halt, 404}, ReqData, State}
						end;
					Id ->
						% Get specific datapoint
						case erlastic_search:get_doc(?INDEX, "datapoint", Id) of
							{error, _} ->
								{{halt, 404}, ReqData, State};
							{ok,{struct, JsonData}} ->
								Datapoint = proplists:get_value(<<"_source">>, JsonData),
								{json_encode(Datapoint), ReqData, State}
						end
				end;
			true ->	
				process_search(ReqData,State, get)	
		end.


%% @doc
%% Function: is_search/2
%% Purpose: Returns true if it is a search POST/GET request.
%% Returns: {true | false}
%% @end
-spec is_search(string()) -> boolean().
is_search(ReqData) ->
		URIList = string:tokens(wrq:path(ReqData), "/"),
		string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search".


%% @doc
%% Function: process_search/3
%% Purpose: Does search for Datapoints for either search done with POST or GET
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%% @end
-spec process_search(ReqData::tuple(), State::string(), term()) ->
		{list(), tuple(), string()}.
process_search(ReqData, State, post) ->
		{Json,_,_} = json_handler(ReqData,State),
		{struct, JsonData} = mochijson2:decode(Json),
		Query = transform(JsonData),
		case erlastic_search:search_limit(?INDEX, "datapoint", Query, 10) of
			{error,Reason} -> {{error,Reason}, ReqData, State};
			{ok,List} -> {true, wrq:set_resp_body(json_encode(List),ReqData),State}
		end;
process_search(ReqData, State, get) ->
		TempQuery = wrq:req_qs(ReqData),
		TransformedQuery = transform(TempQuery),
		case erlastic_search:search_limit(?INDEX, "datapoint", TransformedQuery, 10) of
			{error,Reason} -> {{error,Reason}, ReqData, State};
			{ok,List} -> {json_encode(List),ReqData,State} % May need to convert
		end.


%% @doc
%% Function: transform/1
%% Purpose: Transforms the query into the proper query language
%% Returns: string()
%% @end
-spec transform(tuple()) -> {string()}.
transform([]) -> "";
transform([{Field,Value}|Rest]) when is_binary(Field) andalso is_binary(Value)->
		case Rest of
			[] -> binary_to_list(Field) ++ ":" ++ binary_to_list(Value) ++
					transform(Rest);
			_ -> binary_to_list(Field) ++ ":" ++ binary_to_list(Value) ++ "&" ++
					transform(Rest)
		end;
transform([{Field,Value}|Rest]) ->
		case Rest of
			[] -> Field ++ ":" ++ Value ++ transform(Rest);
			_ -> A= Field ++ ":" ++ Value ++ "&" ++ transform(Rest),
				 erlang:display(A),
				 A
		end.


%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the if from the path.
%% Returns: Id
%% @end
-spec id_from_path(string()) -> string().
id_from_path(RD) ->
    case wrq:path_info(id, RD) of
        undefined ->
            case string:tokens(wrq:disp_path(RD), "/") of
					["datapoints", Id] -> Id;
					_ -> undefined
			end;
        Id -> Id
    end.


%% @doc
%% Function: parse_path/1
%% Purpose: Given a string representation of a search path, the path is split
%% by the '/' token and the return value is a list of tuples [{dir, id}].
%% Returns: [{"directory_name", "id_value"}] | [{Error, Err}] | []
%% @end
-spec parse_path(string()) -> string().
parse_path(Path) ->
		[_|T] = filename:split(Path),
		pair(T).


%% @doc
%% Function: pair/1
%% Purpose: Pairs the values of the input list
%% Returns: A list with paired values
%% @end
-spec pair(list()) -> list().
pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].


%% @doc
%% Function: json_encode/2
%% Purpose: To encode utf8-json WITHOUT converting multi-byte utf8-chars into ASCII '\uXXXX'.
%% Returns: A string with fields and values formatted in a correct way
%% @end
-spec json_encode(string()) -> string().
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).
