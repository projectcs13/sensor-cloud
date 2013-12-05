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
		 delete_resource/2]).


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
	    [{"users", _UserID},{"triggers",_Action}] ->
		{['POST'], ReqData, State};
	    [{"users", _UserId},{"triggers"}] ->
		{['GET'], ReqData, State};
	    [{"triggers",_Action}] ->
		{['POST'], ReqData, State};
	    [{"triggers"}] ->
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



%% Place holder, not sure if we want this to be doable

get_triggers(ReqData, State) ->
    case proplists:get_value('userid', wrq:path_info(ReqData)) of
	undefined ->
	    {ok, JsonStruct} = erlastic_search:search_json(#erls_params{}, ?INDEX, "trigger", "{}"),		       
	    {lib_json:get_field(JsonStruct, "hits.hits"), ReqData, State};
	UserName ->
	    case erlastic_search:get_doc(?INDEX, "user", UserName) of
		{error, {Code, Body}} ->
		    ErrorString = api_help:generate_error(Body, Code),
		    {{halt, Code} = wrq:set_resp_body(ErrorString, ReqData), State};
		{ok, JsonStruct} ->
		    TriggerList = lib_json:get_field(JsonStruct, triggers),			       
		    case proplists:get_value('stream', wrq:path_info(ReqData)) of
			undefined ->			    
			    {lib_json:set_attr(triggers, TriggerList),ReqData, State};
			StreamId ->
			    Fun = fun(X) ->
					  StreamList = lib_json:get_field(X, streams),
					  lists:member(StreamId, StreamList)
				  end,
			    StreamTriggers = lists:filter(Fun, TriggerList),
			    {lib_json:set_attr(triggers, StreamTriggers),ReqData,State}
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
	case proplists:get_value('action', wrq:path_info(ReqData)) of
		"remove" -> delete_resource(ReqData, State);
		"add" ->
			case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
					add_uri(ReqData, State);
				User ->
					add_user(string:to_lower(User),ReqData,State)
			end;
		_ ->
			{true,ReqData,State}
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
	case proplists:get_value('userid', wrq:path_info(ReqData)) of
				undefined ->
					remove_uri(ReqData, State);
				User ->
					remove_user(string:to_lower(User),ReqData,State)
	end.



%% @doc
%% Function: add_uri/2
%% Purpose: Used to handle requests for adding uri's to a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec add_uri(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

add_uri(ReqData, State) ->
	{Json,_,_} = api_help:json_handler(ReqData, State),
	Input = lib_json:get_field(Json, "input"),
	Streams = case lib_json:get_field(Json, "streams") of
				  undefined ->
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
	URI = case lib_json:get_field(Json, "uri") of
			Value3 when is_binary(Value3) ->
				binary_to_list(Value3);
			undefined ->
				error
		  end,
	StreamsQuery = create_stream_query(Streams,[]),
	Query = "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}",	
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % See if the trigger is already in the system
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> undefined;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams)
				   end
		   end,
	case {EsId,Streams,Function,URI} of
		{{error, {Code1, Body1}},_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}}} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{undefined,_,_,_} ->
			NewTrigger = lib_json:set_attrs([{"function",list_to_binary(Function)},{"streams",Streams},{"outputlist","[{}]"},{"outputlist[0].input",Input},{"outputlist[0].output",["{}"]},{"outputlist[0].output[0].output_id",list_to_binary(URI)},{"outputlist[0].output[0].output_type",list_to_binary("uri")}]),
			case erlastic_search:index_doc(?INDEX, "trigger", NewTrigger) of	% Create new triggger if not in the system
				{error,{Code2,Body2}} ->
					ErrorString2 = api_help:generate_error(Body2, Code2),
					{{halt, Code2}, wrq:set_resp_body(ErrorString2, ReqData), State};
				{ok,List2} -> 
					TriggerId = lib_json:to_string(lib_json:get_field(List2, "_id")),
					spawn_link(fun() ->
									   triggersProcess:create(TriggerId, lists:map(fun(A) -> {stream,A} end,Streams), 
															  Function, [{Input,[{uri,URI}]}])
							   end),
					{true, wrq:set_resp_body(lib_json:encode(List2), ReqData), State}
			end;
		{EsId,_,_,_}->
			erlang:display(EsId),
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({add,{Input,{uri,URI}}}),
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
									 "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput && !ctx._source.outputlist[i].output.contains(newoutput)){ctx._source.outputlist[i].output += newoutput; i = ctx._source.outputlist.size();}}\", \"params\":{\"newinput\":" ++  lib_json:to_string(Input)++ ", \"newoutput\":{\"output_id\":\""++URI++"\",\"output_type\":\"uri\"}}}";
								 false ->
									 "{\"script\" : \"ctx._source.outputlist += newelement\", \"params\":{\"newelement\":{\"input\" : "++ lib_json:to_string(Input) ++ ", \"output\":[{\"output_id\":\""++URI++"\",\"output_type\":\"uri\"}]}}}"
							 end,
					case api_help:update_doc(?INDEX, "trigger", EsId, Update) of % Update document in es with the new user
						{error, {Code4, Body4}} -> 
							ErrorString4 = api_help:generate_error(Body4, Code4),
							{{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
						{ok,List4} -> 
							{true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
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
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % See if the trigger is already in the system
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> undefined;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams)
				   end
		   end,
	case {EsId,Streams,Function,Username} of
		{{error, {Code1, Body1}},_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}}} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{undefined,_,_,_} ->
			NewTrigger = lib_json:set_attrs([{"function",list_to_binary(Function)},{"streams",Streams},{"outputlist","[{}]"},{"outputlist[0].input",Input},{"outputlist[0].output",["{}"]},{"outputlist[0].output[0].output_id",list_to_binary(Username)},{"outputlist[0].output[0].output_type",list_to_binary("user")}]),
			case erlastic_search:index_doc(?INDEX, "trigger", NewTrigger) of	% Create new triggger if not in the system
				{error,{Code2,Body2}} ->
					ErrorString2 = api_help:generate_error(Body2, Code2),
					{{halt, Code2}, wrq:set_resp_body(ErrorString2, ReqData), State};
				{ok,List2} -> 
					TriggerId = lib_json:to_string(lib_json:get_field(List2, "_id")),
					spawn_link(fun() ->
									   triggersProcess:create(TriggerId, lists:map(fun(A) -> {stream,A} end,Streams), 
															  Function, [{Input,[{user,Username}]}])
							   end),
					UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
													 {"params","{}"},{"params.newelement","{}"},
													 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
													 {"params.newelement.streams",Streams}]),
					case api_help:update_doc(?INDEX, "user", Username, UserUpdate) of
						{error, {UPCode, UPBody}} -> 
							UPErrorString = api_help:generate_error(UPBody, UPCode),
							{{halt, UPCode}, wrq:set_resp_body(UPErrorString, ReqData), State};
						{ok, _} ->
							{true, wrq:set_resp_body(lib_json:encode(List2), ReqData), State}
					end
			end;
		{EsId,_,_,_}->
			erlang:display(EsId),
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
									 "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput && !ctx._source.outputlist[i].output.contains(newoutput)){ctx._source.outputlist[i].output += newoutput; i = ctx._source.outputlist.size();}}\", \"params\":{\"newinput\":" ++  lib_json:to_string(Input)++ ", \"newoutput\":{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}}}";
								 false ->
									 "{\"script\" : \"ctx._source.outputlist += newelement\", \"params\":{\"newelement\":{\"input\" : "++ lib_json:to_string(Input) ++ ", \"output\":[{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}]}}}"
							 end,
					case api_help:update_doc(?INDEX, "trigger", EsId, Update) of % Update document in es with the new user
						{error, {Code4, Body4}} -> 
							ErrorString4 = api_help:generate_error(Body4, Code4),
							{{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
						{ok,List4} -> 
							UserUpdate = lib_json:set_attrs([{"script",list_to_binary("ctx._source.triggers += newelement")},
															 {"params","{}"},{"params.newelement","{}"},
															 {"params.newelement.function",list_to_binary(Function)}, {"params.newelement.input",Input},
															 {"params.newelement.streams",Streams}]),
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
%% Function: remove_uri/2
%% Purpose: Used to handle requests for removing uri's from a trigger
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec remove_uri(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

remove_uri(ReqData, State) ->
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
	URI = case lib_json:get_field(Json, "uri") of
			Value3 when is_binary(Value3) ->
				binary_to_list(Value3);
			undefined ->
				error
	end,
	StreamsQuery = create_stream_query(Streams,[]),
	Query = "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}",% now finds all triggers where the streams being searched for is a subset of the streams for the trigger when they have the same function 
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Get the es id of the trigger
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   erlang:display(binary_to_list(iolist_to_binary(lib_json:encode(JsonStruct)))),
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> error;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams)
				   end
		   end,
	case {EsId,Streams,Function,URI} of
		{{error, {Code1, Body1}},_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{error,_,_,_} ->
			{{halt, 404}, ReqData, State};
		{_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}}} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{EsId,_,_ ,_}->		
			Return = case erlastic_search:get_doc(?INDEX, "trigger", EsId) of % Update the es document by removing the user
						 {error, {Code3, Body3}} -> 
							 ErrorString3 = api_help:generate_error(Body3, Code3),
							 {{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
						 {ok,JsonStruct2} -> 	 
							 Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
										  true ->
											  "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput){if (ctx._source.outputlist[i].output == newoutputlist) {ctx._source.outputlist.remove((Object) ctx._source.outputlist[i]); i = ctx._source.outputlist.size();} else {for(int k=0;k < ctx._source.outputlist[i].output.size(); k++) {if (ctx._source.outputlist[i].output[k] == newoutput) {ctx._source.outputlist[i].output.remove((Object) ctx._source.outputlist[i].output[k]); k = ctx._source.outputlist[i].output.size();}}}}}\", \"params\":{\"newinput\":\"" ++ lib_json:to_string(Input) ++ "\",  \"newoutputlist\":[{\"output_id\":\""++URI++"\",\"output_type\":\"uri\"}],  \"newoutput\":{\"output_id\":\""++URI++"\",\"output_type\":\"uri\"}}}";
										  false ->
											  erlang:display("Error: input not in file"),
											  "{}"
									  end,
							 case api_help:update_doc(?INDEX, "trigger", EsId, Update) of 
								 {error, {Code4, Body4}} -> 
									 ErrorString4 = api_help:generate_error(Body4, Code4),
									 {{halt, Code4}, wrq:set_resp_body(ErrorString4, ReqData), State};
								 {ok,List4} -> 
									 {true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
							 end
					 end,
			CommandExchange = list_to_binary("command.trigger."++ EsId),
			Msg = term_to_binary({remove,{Input,{uri,URI}}}),
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
	Query = "{\"filter\":{\"term\":{ \"function\":\"" ++ Function ++ "\"}},\"query\":{\"match\":{\"streams\":{\"query\":\"" ++ StreamsQuery ++"\",\"operator\":\"and\"}}}}",% now finds all triggers where the streams being searched for is a subset of the streams for the trigger when they have the same function 
	EsId = case erlastic_search:search_json(#erls_params{},?INDEX, "trigger", Query) of % Get the es id of the trigger
			   {error, {Code, Body}} -> 
				   {error, {Code, Body}};
			   {ok,JsonStruct} ->
				   erlang:display(binary_to_list(iolist_to_binary(lib_json:encode(JsonStruct)))),
				   case lib_json:get_field(JsonStruct, "hits.total") of
					   0 -> error;
					   1 -> lib_json:get_field(JsonStruct, "hits.hits[0]._id");
					   X -> get_es_id(lib_json:get_field(JsonStruct, "hits.hits"),Streams)
				   end
		   end,
	case {EsId,Streams,Function,Username} of
		{{error, {Code1, Body1}},_,_,_} ->
			ErrorString1 = api_help:generate_error(Body1, Code1),
			{{halt, Code1}, wrq:set_resp_body(ErrorString1, ReqData), State};
		{_,error,_,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid stream list should be a valid stream id or a list of valid stream ids", ReqData), State};
		{_,_,error,_} ->
			{{halt, 405}, wrq:set_resp_body("Invalid function", ReqData), State};
		{error,_,_,_} ->
			{{halt, 404}, ReqData, State};
		{_,_,_,error} ->
			{{halt, 405}, wrq:set_resp_body("Error when retriving user_id", ReqData), State};
		{_,_,_,{error, {UCode, UBody}}} ->
			UErrorString = api_help:generate_error(UBody, UCode),
			{{halt, UCode}, wrq:set_resp_body(UErrorString, ReqData), State};
		{EsId,_,_ ,_}->		
			Return = case erlastic_search:get_doc(?INDEX, "trigger", EsId) of % Update the es document by removing the user
						 {error, {Code3, Body3}} -> 
							 ErrorString3 = api_help:generate_error(Body3, Code3),
							 {{halt, Code3}, wrq:set_resp_body(ErrorString3, ReqData), State};
						 {ok,JsonStruct2} -> 	 
							 Update = case lib_json:field_value_exists(JsonStruct2, "_source.outputlist[*].input", Input) of
										  true ->
											  "{\"script\" : \"for(int i=0;i < ctx._source.outputlist.size(); i++){if (ctx._source.outputlist[i].input == newinput){if (ctx._source.outputlist[i].output == newoutputlist) {ctx._source.outputlist.remove((Object) ctx._source.outputlist[i]); i = ctx._source.outputlist.size();} else {for(int k=0;k < ctx._source.outputlist[i].output.size(); k++) {if (ctx._source.outputlist[i].output[k] == newoutput) {ctx._source.outputlist[i].output.remove((Object) ctx._source.outputlist[i].output[k]); k = ctx._source.outputlist[i].output.size();}}}}}\", \"params\":{\"newinput\":\"" ++ lib_json:to_string(Input) ++ "\",  \"newoutputlist\":[{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}],  \"newoutput\":{\"output_id\":\""++Username++"\",\"output_type\":\"user\"}}}";
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
																	  {"params.newelement.streams",Streams}]),
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

create_stream_query([StreamId],Acc) ->
	StreamId ++ Acc;
create_stream_query([StreamId|Rest],Acc) ->
	create_stream_query(Rest," " ++ StreamId ++ Acc).

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
	error;
get_es_id([First|Rest],Streams) ->
	case matches_exactly(lib_json:get_field(First, "_source.streams"),Streams) of
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

