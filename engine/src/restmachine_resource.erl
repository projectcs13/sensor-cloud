%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

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

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['PUT', 'HEAD', 'GET', 'POST', 'DELETE'], ReqData, State}.

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
	Data = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	erlang:display("Parsing JSON"),
	erlang:display(Data),
	{true, ReqData, State}.
%% GET
json_get(ReqData, State) ->
	{ "{'label':'Hello, World!'}", ReqData, State}.

%% To-do : HTTP Caching support w etags / header expiration.
	
