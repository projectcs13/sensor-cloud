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
		[{"users", _UserID}, {"streams", _StreamID},{"triggers",_Func}] ->
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
	StreamId = proplists:get_value('streamid', wrq:path_info(ReqData)),
	Username = proplists:get_value('userid', wrq:path_info(ReqData)),
	Function = proplists:get_value('func', wrq:path_info(ReqData)),
	erlang:display(StreamId),
	CommandExchange = list_to_binary("triggers."++StreamId),
	Msg = term_to_binary({add,Username,Function}),
	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	%% Open channel
	{ok, Channel} = amqp_connection:open_channel(Connection),
	%% Declare exchange
	amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),        
	%% Send
	amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
	{true,ReqData,State}.

delete_resource(ReqData, State) ->
	erlang:display("Got here!"),
	StreamId = proplists:get_value('streamid', wrq:path_info(ReqData)),
	Username = proplists:get_value('userid', wrq:path_info(ReqData)),
	Function = proplists:get_value('func', wrq:path_info(ReqData)),
	erlang:display(StreamId),
	CommandExchange = list_to_binary("triggers."++StreamId),
	Msg = term_to_binary({remove,Username,Function}),
	%% Connect
	{ok, Connection} =
		amqp_connection:start(#amqp_params_network{host = "localhost"}),
	%% Open channel
	{ok, Channel} = amqp_connection:open_channel(Connection),
	%% Declare exchange
	amqp_channel:call(Channel, #'exchange.declare'{exchange = CommandExchange, type = <<"fanout">>}),        
	%% Send
	amqp_channel:cast(Channel, #'basic.publish'{exchange = CommandExchange}, #amqp_msg{payload = Msg}),
	{true,ReqData,State}.






		

