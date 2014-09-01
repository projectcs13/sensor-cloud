%% @author Tomas Sävström <tosa7943@student.uu.se>, Andreas Moreg�rd Haubenwaller
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == triggers ==
%% This module will contain all functions needed to handle
%% http requests done to the webmachine regarding triggers
%%
%% @end

-module(triggers).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, get_triggers/2, process_post/2,
		 delete_resource/2, start_all_triggers_in_es/0]).


-define(ELASTIC_SEARCH_URL, api_help:get_elastic_search_url()).
-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include("field_restrictions.hrl").
-include("debug.hrl").
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
	    [{"users", _UserID},{"streams",_StreamId},{"triggers"}] ->
			{['GET'], ReqData, State};
		[{"users", _UserID},{"vstreams",_VStreamId},{"triggers"}] ->
			{['GET'], ReqData, State};
	    [{"users", _UserID},{"triggers",_Action}] ->
			{['POST'], ReqData, State};
	    [{"users", _UserId},{"triggers"}] ->
			{['GET'], ReqData, State};
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
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
		    case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
				    {ok, JsonStruct} = erlastic_search:search_json(#erls_params{}, ?INDEX, "trigger", "{}"),
			    	{lib_json:set_attr(error, "need to supply a username"), ReqData, State};
				UserName ->
				    case erlastic_search:get_doc(?INDEX, "user", UserName) of
					{error, {Code, Body}} ->
					    ErrorString = api_help:generate_error(Body, Code),
					    {{halt, Code} = wrq:set_resp_body(ErrorString, ReqData), State};
					{ok, JsonStruct} ->
					    TriggerList = lib_json:get_field(JsonStruct, "_source.triggers"),
					    case {proplists:get_value('streamid', wrq:path_info(ReqData)),proplists:get_value('vstreamid', wrq:path_info(ReqData))} of
							{undefined,undefined} ->
								{lib_json:set_attr(triggers, TriggerList),ReqData, State};
							{StreamId,undefined} ->
								Fun = fun(X) ->
											  StreamList = lib_json:get_field(X, streams),
											  lists:member(binary:list_to_bin(StreamId), StreamList)
									  end,
								StreamTriggers = lists:filter(Fun, TriggerList),
								{lib_json:set_attr(triggers, StreamTriggers),ReqData,State};
							{undefined,VStreamId} ->
								Fun = fun(X) ->
											  VStreamList = lib_json:get_field(X, vstreams),
											  lists:member(binary:list_to_bin(VStreamId), VStreamList)
									  end,
								VStreamTriggers = lists:filter(Fun, TriggerList),
								{lib_json:set_attr(triggers, VStreamTriggers),ReqData,State};
							_ ->
								{lib_json:set_attr(triggers, TriggerList),ReqData, State}
					    end
				    end
			end
	end.


%% @doc
%% Function: process_post/2
%% Purpose: Used to handle POST requests by creating new triggers or updating old ones
%% Returns: {Success, ReqData, State}, where Success is true if the post request is
%% successful and false otherwise.
%% @end
-spec process_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
process_post(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			case proplists:get_value('action', wrq:path_info(ReqData)) of
				"remove" -> delete_resource(ReqData, State);
				"add" ->
					{Json,_,_} = api_help:json_handler(ReqData, State),
					User = proplists:get_value('userid', wrq:path_info(ReqData)),
					case lib_json:get_field(Json, "uri") of
						undefined ->
							add_user(string:to_lower(User),ReqData,State);
						URI ->
							add_uri(string:to_lower(User),ReqData, State)
					end;
				_ ->
					{true,ReqData,State}
			end
	end.

%% @doc
%% Function: delete_resource/2
%% Purpose: Used to handle requests for deleting users from the lists of
%%          current triggers

%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
delete_resource(ReqData, State) ->
	case openidc:auth_request(ReqData) of
        {error, Msg} -> {{halt, 498}, wrq:set_resp_body(Msg, ReqData), State};
        {ok, _} ->
			{Json,_,_} = api_help:json_handler(ReqData, State),
			User = proplists:get_value('userid', wrq:path_info(ReqData)),
			case lib_json:get_field(Json, "uri") of
						undefined ->
							remove_user(string:to_lower(User),ReqData,State);
						URI ->
							remove_uri(string:to_lower(User),ReqData, State)

			end
	end.



%% @doc
%% Function: add_uri/3
%% Purpose: Used to handle requests for adding uri's to a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec add_uri(User::string(),ReqData::term(),State::term()) -> {boolean(), term(), term()}.

add_uri(User,ReqData, State) ->
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefined ->
					  error;
				  "" ->
					  [];
				  Value when is_binary(Value) ->
					  case binary_to_list(Value) of
						  "" ->
							  [];
						  _ ->
							  [binary_to_list(Value)]
					  end;
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	VirtualStreams = case lib_json:get_field(Json, "vstreams") of
						 undefiend ->
							 error;
						 "" ->
							 [];
						 VValue when is_binary(VValue) ->
							 case binary_to_list(VValue) of
								 "" ->
									 [];
								 _ ->
									 [binary_to_list(VValue)]
							 end;
						 VList ->
							 lists:map(fun(A) -> binary_to_list(A) end,VList)
					 end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	URI = case lib_json:get_field(Json, "uri") of
			Value3 when is_binary(Value3) ->
				binary_to_list(Value3);
			undefined ->
				error
		  end,
	StreamsQuery = create_stream_query(Streams,[]),
	VirtualStreamsQuery = create_stream_query(VirtualStreams,[]),
	Query = case {Streams,VirtualStreams} of
			   {[],_} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   {_,[]} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   _ ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}"
		   end,
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % See if the trigger is already in the system
			   {error, {Code, Body}} ->
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> undefined;
					   1 -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams);
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams)
				   end
		   end,
	Type = case Streams of
			   [] ->
				   "vstream";
			   _ ->
				   "stream"
		   end,
	case {EsId,Streams,Function,URI,VirtualStreams} of
		{{error, {Code1, Body1}},_,_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{_,_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}},_} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{_,_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid virtual stream list should be a valid virtual stream id or a list of valid virtual stream ids", ReqData), State};
		{undefined,_,_,_,_} ->
			NewTrigger = lib_json:set_attrs([{"function",list_to_binary(Function)},{"streams",Streams},{"vstreams",VirtualStreams},{"type",list_to_binary(Type)},{"outputlist","[{}]"},{"outputlist[0].input",Input},{"outputlist[0].output",["{}"]},{"outputlist[0].output[0].output_id",[list_to_binary(URI),list_to_binary(User)]},{"outputlist[0].output[0].output_type",list_to_binary("uri")}]),
			case erlastic_search:index_doc(?INDEX, "trigger", NewTrigger) of	% Create new triggger if not in the system
				{error,{Code2,Body2}} ->
					ErrorString2 = api_help:generate_error(Body2, Code2),
					{{halt, Code2}, wrq:set_resp_body(ErrorString2, ReqData), State};
				{ok,List2} ->
					TriggerId = lib_json:to_string(lib_json:get_field(List2, "_id")),
					Type = case Streams of
							   [] ->
								   "vstream";
							   _ ->
								   "stream"
						   end,
					spawn_link(fun() ->
									   triggersProcess:create(TriggerId, lists:map(fun(A) -> {stream,A} end,Streams) ++ lists:map(fun(A) -> {vstream,A} end,VirtualStreams),
															  Function, [{Input,[{uri,{URI,User}}]}],Type)
							   end),
					UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
													 {"params","{}"},{"params.newelement","{}"},
													 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
													 {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
													 {"params.newelement.output_type",list_to_binary("uri")},{"params.newelement.output_id",list_to_binary(URI)}]),
					case api_help:update_doc(?INDEX, "user", User, UserUpdate) of
						{error, {UPCode, UPBody}} ->
							UPErrorString = api_help:generate_error(UPBody, UPCode),
							{{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
						{ok, _} ->
							{true, wrq:set_resp_body(lib_json:encode(List2), ReqData), State}
					end
			end;
		{EsId,_,_,_,_}->
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({add,{Input,{uri,{URI,User}}}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),      % Update triggersProcess running of new user
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			case erlastic_search:get_doc(?INDEX, "trigger", EsId) of
				{error, {Code3, Body3}} ->
					ErrorString3 = api_help:generate_error(Body3, Code3),
					{{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
				{ok,JsonStruct2} ->
					Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
								 true ->
									 "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput && !ctx._source.outputlist[i].output.contains(newoutput)){ctx._source.outputlist[i].output += newoutput; i = ctx._source.outputlist.size();}}\", \"params\":{\"newinput\":" ++  input_to_string(Input)++ ", \"newoutput\":{\"output_id\":[\""++URI++"\",\""++ User++"\"],\"output_type\":\"uri\"}}}";
								 false ->
									 "{\"script\" : \"ctx._source.outputlist += newelement\", \"params\":{\"newelement\":{\"input\" : "++ input_to_string(Input) ++ ", \"output\":[{\"output_id\":[\""++URI++"\",\""++ User++"\"],\"output_type\":\"uri\"}]}}}"
							 end,
					case api_help:update_doc(?INDEX, "trigger", EsId, Update) of % Update document in es with the new user
						{error, {Code4, Body4}} ->
							ErrorString4 = api_help:generate_error(Body4, Code4),
							{{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
						{ok,List4} ->
							UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
															 {"params","{}"},{"params.newelement","{}"},
															 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
															 {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
															 {"params.newelement.output_type",list_to_binary("uri")},{"params.newelement.output_id",list_to_binary(URI)}]),
							case api_help:update_doc(?INDEX, "user", User, UserUpdate) of
								{error, {UPCode, UPBody}} ->
									UPErrorString = api_help:generate_error(UPBody, UPCode),
									{{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
								{ok, _} ->
									{true, wrq:set_resp_body(lib_json:encode(List4), ReqData), State}
							end
					end
			end

	end.



%% @doc
%% Function: add_user/3
%% Purpose: Used to handle requests for adding users to a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec add_user(User::string(),ReqData::term(),State::term()) -> {boolean(), term(), term()}.

add_user(User, ReqData, State) ->
	Username = User,
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefiend ->
					  error;
				  "" ->
					  [];
				  Value when is_binary(Value) ->
					  case binary_to_list(Value) of
						  "" ->
							  [];
						  _ ->
							  [binary_to_list(Value)]
					  end;
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	VirtualStreams = case lib_json:get_field(Json, "vstreams") of
				  undefiend ->
					  error;
				  "" ->
					  [];
				  VValue when is_binary(VValue) ->
					  case binary_to_list(VValue) of
						  "" ->
							  [];
						  _ ->
							  [binary_to_list(VValue)]
					  end;
				  VList ->
					  lists:map(fun(A) -> binary_to_list(A) end,VList)
			  end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	StreamsQuery = create_stream_query(Streams,[]),
	VirtualStreamsQuery = create_stream_query(VirtualStreams,[]),
	Query = case {Streams,VirtualStreams} of
			   {[],_} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   {_,[]} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   _ ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}"
		   end,
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % See if the trigger is already in the system
			   {error, {Code, Body}} ->
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> undefined;
					   1 -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams);
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams)
				   end
		   end,
	Type = case Streams of
			   [] ->
				   "vstream";
			   _ ->
				   "stream"
		   end,
	case {EsId,Streams,Function,Username,VirtualStreams} of
		{{error, {Code1, Body1}},_,_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{_,_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}},_} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{_,_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid virtual stream list should be a valid virtual stream id or a list of valid virtual stream ids", ReqData), State};
		{undefined,_,_,_,_} ->
			NewTrigger = lib_json:set_attrs([{"function",list_to_binary(Function)},{"streams",Streams},{"vstreams",VirtualStreams},{"type",list_to_binary(Type)},{"outputlist","[{}]"},{"outputlist[0].input",Input},{"outputlist[0].output",["{}"]},{"outputlist[0].output[0].output_id",list_to_binary(Username)},{"outputlist[0].output[0].output_type",list_to_binary("user")}]),
			case erlastic_search:index_doc(?INDEX, "trigger", NewTrigger) of	% Create new triggger if not in the system
				{error,{Code2,Body2}} ->
					ErrorString2 = api_help:generate_error(Body2, Code2),
					{{halt, Code2}, wrq:set_resp_body(ErrorString2, ReqData), State};
				{ok,List2} ->
					TriggerId = lib_json:to_string(lib_json:get_field(List2, "_id")),
					spawn_link(fun() ->
									   triggersProcess:create(TriggerId, lists:map(fun(A) -> {stream,A} end,Streams) ++ lists:map(fun(A) -> {vstream,A} end,VirtualStreams),
															  Function, [{Input,[{user,Username}]}],Type)
							   end),
					UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
													 {"params","{}"},{"params.newelement","{}"},
													 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
													 {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
													 {"params.newelement.output_type",list_to_binary("user")},{"params.newelement.output_id",list_to_binary(Username)}]),
					case api_help:update_doc(?INDEX, "user", Username, UserUpdate) of
						{error, {UPCode, UPBody}} ->
							UPErrorString = api_help:generate_error(UPBody, UPCode),
							{{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
						{ok, _} ->
							{true, wrq:set_resp_body(lib_json:encode(List2), ReqData), State}
					end
			end;
		{EsId,_,_,_,_}->
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({add,{Input,{user,Username}}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),      % Update triggersProcess running of new user
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			case erlastic_search:get_doc(?INDEX, "trigger", EsId) of
				{error, {Code3, Body3}} ->
					ErrorString3 = api_help:generate_error(Body3, Code3),
					{{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
				{ok,JsonStruct2} ->
					Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
								 true ->
									 "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput && !ctx._source.outputlist[i].output.contains(newoutput)){ctx._source.outputlist[i].output += newoutput; i = ctx._source.outputlist.size();}}\", \"params\":{\"newinput\":" ++  input_to_string(Input)++ ", \"newoutput\":{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}}}";
								 false ->
									 "{\"script\" : \"ctx._source.outputlist += newelement\", \"params\":{\"newelement\":{\"input\" : "++ input_to_string(Input) ++ ", \"output\":[{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}]}}}"
							 end,
					case api_help:update_doc(?INDEX, "trigger", EsId, Update) of % Update document in es with the new user
						{error, {Code4, Body4}} ->
							ErrorString4 = api_help:generate_error(Body4, Code4),
							{{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
						{ok,List4} ->
							UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
															 {"params","{}"},{"params.newelement","{}"},
															 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
															 {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
															 {"params.newelement.output_type",list_to_binary("user")},{"params.newelement.output_id",list_to_binary(Username)}]),
							case api_help:update_doc(?INDEX, "user", Username, UserUpdate) of
								{error, {UPCode, UPBody}} ->
									UPErrorString = api_help:generate_error(UPBody, UPCode),
									{{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
								{ok, _} ->
									{true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
							end
					end
			end

	end.



%% @doc
%% Function: remove_uri/3
%% Purpose: Used to handle requests for removing uri's from a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec remove_uri(User::string(),ReqData::term(),State::term()) -> {boolean(), term(), term()}.

remove_uri(User,ReqData, State) ->
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefiend ->
					  error;
				  "" ->
					  [];
				  Value when is_binary(Value) ->
					  case binary_to_list(Value) of
						  "" ->
							  [];
						  _ ->
							  [binary_to_list(Value)]
					  end;
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	VirtualStreams = case lib_json:get_field(Json, "vstreams") of
						 undefiend ->
							 error;
						 "" ->
							 [];
						 VValue when is_binary(VValue) ->
							 case binary_to_list(VValue) of
								 "" ->
									 [];
								 _ ->
									 [binary_to_list(VValue)]
							 end;
						 VList ->
							 lists:map(fun(A) -> binary_to_list(A) end,VList)
					 end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	URI = case lib_json:get_field(Json, "uri") of
			Value3 when is_binary(Value3) ->
				binary_to_list(Value3);
			undefined ->
				error
	end,
	StreamsQuery = create_stream_query(Streams,[]),
	VirtualStreamsQuery = create_stream_query(VirtualStreams,[]),
	Query = case {Streams,VirtualStreams} of
			   {[],_} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   {_,[]} ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}";
			   _ ->
				   "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}"
		   end,
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Get the es id of the trigger
			   {error, {Code, Body}} ->
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   erlang:display(binary_to_list(iolist_to_binary(lib_json:encode(JsonStruct)))),
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> error;
					   1 -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams);
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams)
				   end
		   end,
	Type = case Streams of
			   [] ->
				   "vstream";
			   _ ->
				   "stream"
		   end,
	case {EsId,Streams,Function,URI,VirtualStreams} of
		{{error, {Code1, Body1}},_,_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{error,_,_,_,_} ->
			{{halt, 404}, ReqData, State};
		{_,_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}},_} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{_,_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid virtual stream list should be a valid virtual stream id or a list of valid virtual stream ids", ReqData), State};
		{EsId,_,_ ,_,_}->
			Return = case erlastic_search:get_doc(?INDEX, "trigger", EsId) of % Update the es document by removing the user
						 {error, {Code3, Body3}} ->
							 ErrorString3 = api_help:generate_error(Body3, Code3),
							 {{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
						 {ok,JsonStruct2} ->
							 Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
										  true ->
											  "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput){if (ctx._source.outputlist[i].output == newoutputlist) {ctx._source.outputlist.remove((Object) ctx._source.outputlist[i]); i = ctx._source.outputlist.size();} else {for(int k=0;k < ctx._source.outputlist[i].output.size(); k++) {if (ctx._source.outputlist[i].output[k] == newoutput) {ctx._source.outputlist[i].output.remove((Object) ctx._source.outputlist[i].output[k]); k = ctx._source.outputlist[i].output.size();}}}}}\", \"params\":{\"newinput\":\"" ++ input_to_string(Input) ++ "\",  \"newoutputlist\":[{\"output_id\":[\""++URI++"\",\""++ User++"\"],\"output_type\":\"uri\"}],  \"newoutput\":{\"output_id\":[\""++URI++"\",\""++ User++"\"],\"output_type\":\"uri\"}}}";
										  false ->
											  erlang:display("Error: input not in file"),
											  "{}"
									  end,
							 case api_help:update_doc(?INDEX, "trigger", EsId, Update) of
								 {error, {Code4, Body4}} ->
									 ErrorString4 = api_help:generate_error(Body4, Code4),
									 {{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
								 {ok,List4} ->
									 UserUpdate = lib_json:set_attrs([{"script",list_to_binary("for(int i=0;i < ctx._source.triggers.size(); i++){if (ctx._source.triggers[i] == newelement){ctx._source.triggers.remove((Object) ctx._source.triggers[i]); i = ctx._source.triggers.size();}}")},
																	  {"params","{}"},{"params.newelement","{}"},
																	  {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
																	  {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
																	  {"params.newelement.output_type",list_to_binary("uri")},{"params.newelement.output_id",list_to_binary(URI)}]),
									 case api_help:update_doc(?INDEX, "user", User, UserUpdate) of
										 {error, {UPCode, UPBody}} ->
											 UPErrorString = api_help:generate_error(UPBody, UPCode),
											 {{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
										 {ok, _} ->
											 {true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
									 end
							 end
					 end,
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({remove,{Input,{uri,{URI,User}}}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),    % Update the triggersProcess by sending a message
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			Return
	end.




%% @doc
%% Function: remove_user/3
%% Purpose: Used to handle requests for removing users from a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec remove_user(User::string(),ReqData::term(),State::term()) -> {boolean(), term(), term()}.

remove_user(User, ReqData, State) ->
	Username = User,
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefiend ->
					  error;
				  "" ->
					  [];
				  Value when is_binary(Value) ->
					  case binary_to_list(Value) of
						  "" ->
							  [];
						  _ ->
							  [binary_to_list(Value)]
					  end;
				  List ->
					  lists:map(fun(A) -> binary_to_list(A) end,List)
			  end,
	VirtualStreams = case lib_json:get_field(Json, "vstreams") of
						 undefiend ->
							 error;
						 "" ->
							 [];
						 VValue when is_binary(VValue) ->
							 case binary_to_list(VValue) of
								 "" ->
									 [];
								 _ ->
									 [binary_to_list(VValue)]
							 end;
						 VList ->
							 lists:map(fun(A) -> binary_to_list(A) end,VList)
					 end,
	Function = case lib_json:get_field(Json, "function") of
				   undefined ->
					   error;
				   Value2 ->
					   binary_to_list(Value2)
			   end,
	StreamsQuery = create_stream_query(Streams,[]),
	VirtualStreamsQuery = create_stream_query(VirtualStreams,[]),
	Query = case {Streams,VirtualStreams} of
				{[],_} ->
					"{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}";
				{_,[]} ->
					"{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}";
				_ ->
					"{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}},\"query\":{\"match\":{\"vstreams\":{\"query\":\"" ++ VirtualStreamsQuery ++"\",\"operator\":\"and\"}}}}"
			end,
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Get the es id of the trigger
			   {error, {Code, Body}} ->
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> error;
					   1 -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams++ VirtualStreams);
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams ++ VirtualStreams)
				   end
		   end,
	Type = case Streams of
			   [] ->
				   "vstream";
			   _ ->
				   "stream"
		   end,
	case {EsId,Streams,Function,Username,VirtualStreams} of
		{{error, {Code1, Body1}},_,_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{error,_,_,_,_} ->
			{{halt, 404}, ReqData, State};
		{_,_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}},_} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{_,_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Invalid virtual stream list should be a valid virtual stream id or a list of valid virtual stream ids", ReqData), State};
		{EsId,_,_ ,_,_}->
			Return = case erlastic_search:get_doc(?INDEX, "trigger", EsId) of % Update the es document by removing the user
						 {error, {Code3, Body3}} ->
							 ErrorString3 = api_help:generate_error(Body3, Code3),
							 {{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
						 {ok,JsonStruct2} ->
							 Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
										  true ->
											  "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput){if (ctx._source.outputlist[i].output == newoutputlist) {ctx._source.outputlist.remove((Object) ctx._source.outputlist[i]); i = ctx._source.outputlist.size();} else {for(int k=0;k < ctx._source.outputlist[i].output.size(); k++) {if (ctx._source.outputlist[i].output[k] == newoutput) {ctx._source.outputlist[i].output.remove((Object) ctx._source.outputlist[i].output[k]); k = ctx._source.outputlist[i].output.size();}}}}}\", \"params\":{\"newinput\":\"" ++ input_to_string(Input) ++ "\",  \"newoutputlist\":[{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}],  \"newoutput\":{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}}}";
										  false ->
											  erlang:display("Error: input not in file"),
											  "{}"
									  end,
							 case api_help:update_doc(?INDEX, "trigger", EsId, Update) of
								 {error, {Code4, Body4}} ->
									 ErrorString4 = api_help:generate_error(Body4, Code4),
									 {{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
								 {ok,List4} ->
									 UserUpdate = lib_json:set_attrs([{"script",list_to_binary("for(int i=0;i < ctx._source.triggers.size(); i++){if (ctx._source.triggers[i] == newelement){ctx._source.triggers.remove((Object) ctx._source.triggers[i]); i = ctx._source.triggers.size();}}")},
																	  {"params","{}"},{"params.newelement","{}"},
																	  {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
																	  {"params.newelement.streams",Streams},{"params.newelement.vstreams",VirtualStreams},{"params.newelement.type",list_to_binary(Type)},
																	  {"params.newelement.output_type",list_to_binary("user")},{"params.newelement.output_id",list_to_binary(Username)}]),
									 case api_help:update_doc(?INDEX, "user", Username, UserUpdate) of
										 {error, {UPCode, UPBody}} ->
											 UPErrorString = api_help:generate_error(UPBody, UPCode),
											 {{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
										 {ok, _} ->
											 {true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
									 end
							 end
					 end,
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({remove,{Input,{user,Username}}}),
			%% Connect, now assumes local host
			{ok, Connection} =
				amqp_connection:start(#amqp_params_network{host = "localhost"}),
			%% Open channel
			{ok, Channel} = amqp_connection:open_channel(Connection),
			%% Declare exchange
			amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),    % Update the triggersProcess by sending a message
			%% Send
			amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
			Return
	end.

%% @doc
%% Function: create_stream_query/2
%% Purpose: Used to create the query string with
%%          all streamids in the list separated by space
%% Returns: The created query string
%% @end
-spec create_stream_query(StreamIdList::[string()],Acc::string()) -> string().
create_stream_query([],Acc) ->
	Acc;
create_stream_query([StreamId],Acc) ->
	StreamId ++ Acc;
create_stream_query([StreamId|Rest],Acc) ->
	create_stream_query(Rest," " ++ StreamId ++ Acc).


%% @doc
%% Function: input_to_string/1
%% Purpose: Used to create the a string from the input
%% Returns: The created string
%% @end
-spec input_to_string(term()) -> string().

input_to_string(Input) when is_list(Input) ->
	String = lists:foldr(fun(A,Acc) -> case is_integer(A) of true -> integer_to_list(A) ++","++ Acc; false -> float_to_list(A, [{decimals, 10}, compact]) ++","++ Acc end end,[],Input),
	"[" ++ string:substr(String,1,length(String)-1) ++ "]";
input_to_string(Input) ->
	lib_json:to_string(Input).

%% @doc
%% Function: get_es_id/2
%% Purpose: Used to get the id of the document that
%%          as the given list of streams and no
%%          more
%% Returns: A es id if there is a document that contains,
%%          exaclty the list given and nothing more
%%          error otherwise
%% @end
-spec get_es_id(DocumentList::list(),StreamsList::string()) -> binary() | error.

get_es_id([],_Streams) ->
	undefined;
get_es_id([First|Rest],Streams) ->
	case matches_exactly(lib_json:get_field(First, "_source.streams") ++ lib_json:get_field(First, "_source.vstreams"),Streams) of
		true ->
			lib_json:get_field(First, "_id");
		false ->
			  get_es_id(Rest,Streams)
	end.


%% @doc
%% Function: matches_exactly/2
%% Purpose: Used check if the streams in the list are
%%          the streams in the document, and that
%%          there are no more streams then them
%% Returns: returns true if the given list from the
%%          document contains only the streams in the list
%%          and no more, false otherwise
%% @end
-spec matches_exactly(DocumentStreamsList::list(),StreamsList::string()) -> boolean().

matches_exactly([],[]) ->
	true;
matches_exactly(List,[]) ->
	false;
matches_exactly([],List) ->
	false;
matches_exactly([First|Rest],Streams) when is_binary(First) ->
	TestId = binary_to_list(First),
	case lists:member(TestId, Streams) of
		true ->
			matches_exactly(Rest,lists:delete(TestId, Streams));
		false ->
			false
	end;
matches_exactly([First|Rest],Streams) ->
	case lists:member(First, Streams) of
		true ->
			matches_exactly(Rest,lists:delete(First, Streams));
		false ->
			false
	end.


%% @doc
%% Function: start_all_triggers_in_es/0
%% Purpose: Used to start all triggers in ES
%%          when the function is called
%% Returns: ok if it managed to start everything
%%          otherwise error
%% @end
-spec start_all_triggers_in_es() -> ok | error.

start_all_triggers_in_es() ->
	AmountQuery = "{\"query\" : {\"match_all\" : {}}}",
	case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", AmountQuery) of % Maybe wanna take more
		{error, {_Code, _Body}} ->
			erlang:display("Error when starting up triggers"),
			error;
		{ok,JsonStruct} ->
			NumberOfTriggers = lib_json:get_field(JsonStruct, "hits.total"),
			TriggerQuery = "{\"size\": " ++ integer_to_list(NumberOfTriggers) ++ ",\"query\" : {\"match_all\" : {}}}",
			case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", TriggerQuery) of % Maybe wanna take more
				{error, {_Code2, _Body2}} ->
					erlang:display("Error when starting up triggers"),
					error;
				{ok,JsonStruct2} ->
					Triggers = lists:map(fun(A) -> {lib_json:to_string(lib_json:get_field(A, "_id")),lib_json:get_field(A, "_source")} end,lib_json:get_field(JsonStruct2, "hits.hits")),
					ParsedTriggers = parse_triggers(Triggers,[]),
					start_processes(ParsedTriggers)
			end
	end.

%% @doc
%% Function: parse_triggers/2
%% Purpose: Used to parse the triggers from
%%          json objects to something that can
%%          be used to start the trigger processes
%% Returns: a list of trigger attributes, where all
%%          attributes for a trigger is in a tuple
%% @end
-spec parse_triggers(TriggerList::list(),Accumelator::list()) -> list().

parse_triggers([],Acc) ->
	Acc;
parse_triggers([{Id,Trigger}|Rest],Acc) ->
	Function = lib_json:to_string(lib_json:get_field(Trigger, "function")),
	Streams = lists:map(fun(A) -> lib_json:to_string(A) end, lib_json:get_field(Trigger, "streams")),
	VStreams = lists:map(fun(A) -> lib_json:to_string(A) end, lib_json:get_field(Trigger, "vstreams")),
	InputList = parse_outputlist(lib_json:get_field(Trigger, "outputlist"),[]),
	Type = lib_json:to_string(lib_json:get_field(Trigger, "type")),
	NewElement = {{id,Id},{function,Function},{inputlist,InputList},{streams,Streams},{vstreams,VStreams},{type,Type}},
	parse_triggers(Rest,[NewElement|Acc]).

%% @doc
%% Function: parse_outputlist/2
%% Purpose: Used to parse the outputlist from
%%          json objects to something that can
%%          be used to start the trigger processes
%% Returns: a list of outputlists
%% @end
-spec parse_outputlist(TriggerList::list(),Accumelator::list()) -> list().

parse_outputlist([],Acc) ->
	Acc;
parse_outputlist([First|Rest],Acc) ->
	Input = lib_json:get_field(First, "input"),
	Output = lists:map(fun(A) -> get_output(A,lib_json:to_string(lib_json:get_field(A, "output_type"))) end, lib_json:get_field(First, "output")),
	NewElement = {Input,Output},
	parse_outputlist(Rest,[NewElement|Acc]).

%% @doc
%% Function: parse_outputlist/2
%% Purpose: Used to parse the output id
%% Returns: an tuple that is either {user,UserId} or {uri,{URI,UserId}}
%% @end
-spec get_output(Json::string(),OutputId::string()) -> {user,string()} | {uri,{string(),string()}}.

get_output(Json,"user") ->
	{user,lib_json:to_string(lib_json:get_field(Json, "output_id"))};
get_output(Json,"uri") ->
	{uri,{lib_json:to_string(lib_json:get_field(Json, "output_id[0]")),lib_json:to_string(lib_json:get_field(Json, "output_id[1]"))}}.
%% @doc
%% Function: start_processes/1
%% Purpose: Used to start processes for the
%%          list of triggers that it is given
%% Returns: ok
%% @end
-spec start_processes(TriggerList::list()) -> ok.

start_processes([]) ->
	ok;
start_processes([{{id,Id},{function,Function},{inputlist,InputList},{streams,Streams},{vstreams,VStreams},{type,Type}}|Rest]) ->
	spawn_link(fun() ->
					   triggersProcess:create(Id, lists:map(fun(A) -> {stream,A} end,Streams) ++ lists:map(fun(A) -> {vstream,A} end,VStreams),
											  Function, InputList,Type)
			   end),
	start_processes(Rest).

