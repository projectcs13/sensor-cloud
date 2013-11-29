%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams ==
%% This module will contain all functions needed to handle 
%% http requests done to the webmachine regarding streams 
%%
%% @end

-module(triggers).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, process_post/2,
		 delete_resource/2]).


-define(ELASTIC_SEARCH_URL, api_help:get_elastic_search_url()).
-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include("field_restrictions.hrl").
-include_lib("amqp_client.hrl").

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
%% Purpose: Used to define what methods are allowed one the given URI's.
%% Returns: {List, ReqData, State}, where list is the allowed methods for the given URI. 
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.

allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"users", _UserID},{"triggers"}] ->
			{['POST','DELETE'], ReqData, State};
		[error] ->
		    {[], ReqData, State} 
	end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::term(),State::term()) -> {list(), term(), term()}.

content_types_provided(ReqData, State) ->
	{[{"application/json", get_triggers}], ReqData, State}.

%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::term(),State::term()) -> {list(), term(), term()}.

content_types_accepted(ReqData, State) ->
	{[{"application/json", process_post}], ReqData, State}.





get_triggers(ReqData, State) ->
	{"ok",ReqData,State}.


process_post(ReqData, State) ->
	Username = proplists:get_value('userid', wrq:path_info(ReqData)),
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefiend ->
					  error;
				  Value when is_binary(Value) ->
					  [binary_to_list(Value)];
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	StreamsQuery = create_stream_query(Streams,[]),
	Query = "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}",	
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Maybe wanna take more
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> undefined;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> erlang:display(X), error
				   end
		   end,
	case {EsId,Streams,Function} of
		{{error, {Code1, Body1}},_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{undefined,_,_} ->
			NewTrigger = lib_json:set_attrs([{"function",list_to_binary(Function)},{"streams",Streams},{"outputlist","[{}]"},{"outputlist[0].input",Input},{"outputlist[0].output",[Username]}]),
			case erlastic_search:index_doc(?INDEX, "trigger", NewTrigger) of	
				{error,{Code2,Body2}} ->
					ErrorString2 = api_help:generate_error(Body2, Code2),
					{{halt, Code2}, wrq:set_resp_body(ErrorString2, ReqData), State};
				{ok,List2} -> 
					TriggerId = lib_json:to_string(lib_json:get_field(List2, "_id")),
					spawn_link(fun() ->
									   triggersProcess:create(TriggerId, lists:map(fun(A) -> {stream,A} end,Streams), 
															  Function, [{Input,[Username]}])
							   end),
					{true, wrq:set_resp_body(lib_json:encode(List2), ReqData), State}
			end;
		{EsId,_,_ }->			

			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({add,{Input,Username}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),        
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			case erlastic_search:get_doc(?INDEX, "trigger", EsId) of 
				{error, {Code3, Body3}} -> 
					ErrorString3 = api_help:generate_error(Body3, Code3),
					{{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
				{ok,JsonStruct2} -> 	 
					Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
								 true ->
									 "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput){ctx._source.outputlist[i].output += newoutput; i = ctx._source.outputlist.size();}}\", \"params\":{\"newinput\":" ++  lib_json:to_string(Input)++ ", \"newoutput\": \""++ Username ++"\"}}";
								 false ->
									 "{\"script\" : \"ctx._source.outputlist += newelement\", \"params\":{\"newelement\":{\"input\" : "++ lib_json:to_string(Input) ++ ", \"output\":[\"" ++ Username ++"\"]}}}"
							 end,
					case api_help:update_doc(?INDEX, "trigger", EsId, Update) of 
						{error, {Code4, Body4}} -> 
							ErrorString4 = api_help:generate_error(Body4, Code4),
		            		{{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
						{ok,List4} -> 
							{true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
					end
			end

	end.


%{"script" : "ctx._source.outputlist.input.contains(newinput) ? (ctx._source.outputlist.output += user ) : ctx.op = \"none\"","params" : {"newinput" :  7, "user" : "Steine"}}

delete_resource(ReqData, State) ->
	Username = proplists:get_value('userid', wrq:path_info(ReqData)),
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefiend ->
					  error;
				  Value when is_binary(Value) ->
					  [binary_to_list(Value)];
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	StreamsQuery = create_stream_query(Streams,[]),
	Query = "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}",	
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Maybe wanna take more
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> error;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> error
				   end
		   end,
	case {EsId,Streams,Function} of
		{{error, {Code1, Body1}},_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{error,_,_} ->
			{{halt, 404}, ReqData, State};
		{EsId,_,_ }->			
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({remove,{Input,Username}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),        
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			{true,ReqData,State}
	end.


create_stream_query([StreamId],Acc) ->
	StreamId ++ Acc;
create_stream_query([StreamId|Rest],Acc) ->
	create_stream_query(Rest," " ++ StreamId ++ Acc).



