%% @author Jacob Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /datapoints

-module(datapoints).
-export([init/1, allowed_methods/2, content_types_provided/2,
		 process_post/2, get_datapoint/2]).

-include("webmachine.hrl").
-include_lib("erlastic_search.hrl").
-include_lib("amqp_client.hrl").
-include_lib("pubsub.hrl").

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
		[{"streams", _Id}, {"data", "_search"}] ->
			{['POST','GET'], ReqData, State};
		[{"streams", _Id}, {"data"}] ->
			{['GET', 'POST', 'DELETE'], ReqData, State};
		[error] ->
				{[], ReqData, State}
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
	case api_help:is_search(ReqData) of
			false ->
				{DatapointJson,_,_} = api_help:json_handler(ReqData, State),
				Id = id_from_path(ReqData),
				case Id of
					undefined -> {{halt, 404}, ReqData, State};
					_ ->
						FinalJson = api_help:add_field(DatapointJson, "streamid", Id),
						case erlastic_search:index_doc(?INDEX, "datapoint", FinalJson) of
							{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ lib_json:encode(Reason) ++ "\"}", ReqData), State};
							{ok,List} -> 
								io:format("RESPONSE: ~p",[FinalJson]),
								%% Create Message
								%Msg = term_to_binary(#'datapoint'{id = Id, timestamp = lib_json:get_field(FinalJson,"timestamp"), value = lib_json:get_field(FinalJson,"value")}),
								Msg = list_to_binary(FinalJson),

								StreamExchange = list_to_binary("streams."++Id),
								%% Connect
                                        			{ok, Connection} =
                                                		amqp_connection:start(#amqp_params_network{host = "localhost"}),
                                        			%% Open channel
                                        			{ok, Channel} = amqp_connection:open_channel(Connection),
                                        			%% Declare exchange
                                        			amqp_channel:call(Channel, #'exchange.declare'{exchange = StreamExchange, type = <<"fanout">>}),	
                                        			%% Send
                                        			amqp_channel:cast(Channel, #'basic.publish'{exchange = StreamExchange}, #amqp_msg{payload = Msg}),
								
								{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
						end
				end;
			true ->
				process_search(ReqData,State, post)	
	end.


%% @doc
%% Function: get_datapoint/2
%% Purpose: Returns the JSON representation of a json-object or multiple json-objects.
%% Returns: {true, ReqData, State} | {false, ReqData, State}
%% @end
-spec get_datapoint(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
get_datapoint(ReqData, State) ->
		case api_help:is_search(ReqData) of
			false ->
				Id = id_from_path(ReqData),			
				case erlastic_search:search_limit(?INDEX, "datapoint", "streamid:" ++ Id, 100) of
					{ok, Result} ->
						EncodedResult = lib_json:encode(Result),
						case re:run(EncodedResult, "\"max_score\":null", [{capture, first, list}]) of
							{match, _} -> {{halt, 404}, ReqData, State};
							nomatch -> FinalJson = lib_json:get_list_and_add_id(Result),
						       {FinalJson, ReqData, State}
						end;
					_ -> {{halt, 404}, ReqData, State}
				end; 
			true ->
				process_search(ReqData,State, get)	
		end.


%% @doc
%% Function: process_search/3
%% Purpose: Does search for Datapoints for either search done with POST or GET
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}

%the POST range query should be structed as follows:
%	curl -XPOST http://localhost:8000/streams/1/data/_search -d '{
%		"size" : 100,
%		query:{
% 		   "filtered" : {
% 		       "query" : {
%  		          "term" : { "streamid" : Id }
%  		      }, "filter" : {  "range" : {    "timestamp" : {"gte" : timestampFromValue,"lte" : timestampToValue}}}}
%		 },"sort" : [{"timestamp" : {"order" : "asc"}}]  }'
%the GET range query should be structed as follows:
%	curl -XGET http://localhost:8000/streams/Id/data/_search    --   to return all the datapoints of the current stream
%or	curl -XGET http://localhost:8000/streams/Id/data/_search\?timestampFrom\=timestampFromValue\&timestampTo\=timestampToValue    --   for range query
%or 	curl -XGET http://localhost:8000/streams/Id/data/_search\?timestampFrom\=timestampFromValue   --   for lower bounded only range qquery
%% @end
-spec process_search(ReqData::tuple(), State::string(), term()) ->
		{list(), tuple(), string()}.
process_search(ReqData, State, post) ->
		{Json,_,_} = api_help:json_handler(ReqData,State),
		case erlastic_search:search_json(#erls_params{},?INDEX, "datapoint", Json) of
				{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ lib_json:encode(Reason) ++ "\"}", ReqData), State};
				{ok,JsonStruct} ->
						       FinalJson = lib_json:get_list_and_add_id(JsonStruct),
						       {true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State}
		end;
process_search(ReqData, State, get) ->
		TempQuery = wrq:req_qs(ReqData),
		Id = id_from_path(ReqData),
		case TempQuery of
			[] ->   
				case erlastic_search:search_limit(?INDEX, "datapoint","streamid:" ++ Id ++ "&sort=timestamp:asc", 100) of
					{error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ lib_json:encode(Reason) ++ "\"}", ReqData), State};
                	{ok,JsonStruct} ->
						       FinalJson = lib_json:get_list_and_add_id(JsonStruct),
						       {FinalJson, ReqData, State}
			 	end;
			_ ->
				TransformedQuery="streamid:" ++ Id ++ transform(TempQuery) ++ "&sort=timestamp:asc",
				case erlastic_search:search_limit(?INDEX, "datapoint",TransformedQuery, 100) of
					{error,Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ lib_json:encode(Reason) ++ "\"}", ReqData), State};
                	{ok,JsonStruct} ->
						       FinalJson = lib_json:get_list_and_add_id(JsonStruct),
						       {FinalJson, ReqData, State}
				end
		end.


%% @doc
%% Function: transform/1
%% Purpose: Transforms the query into the proper query language
%% Returns: string()
%% @end
-spec transform(tuple()) -> {string()}.
transform([]) -> "";
transform([{Field,Value}|Rest]) when is_binary(Field) andalso is_binary(Value)->
	transform([{binary_to_list(Field), binary_to_list(Value)}]) ++ transform(Rest);
transform([{Field,Value}|Rest]) ->
	case Field of 
		"timestampFrom" ->
				case Rest of
					[{"timestampTo",ValueTo}] -> "%20AND%20" ++ "timestamp:[" ++ Value ++ "+TO+" ++ ValueTo ++ "]";
					[] -> "%20AND%20" ++ "timestamp:[" ++ Value ++ "+TO+*" ++ "]"
				end;
		_ ->	
				case Rest of
					[] -> "%20AND%20" ++ Field ++ ":" ++ Value;
					_ -> "%20AND%20" ++ Field ++ ":" ++ Value ++ transform(Rest)
				end
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
					["streams", Id, "data"] -> Id;
					_ -> undefined
			end;
		Id -> Id
	end.
