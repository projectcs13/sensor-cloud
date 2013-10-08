%% @author Tholsgård Gabriel, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poller ==
%% this module implements the functionalities of the poller, which communicates with the external
%% resources
%% @end
-module(poller).
-include("resource.hrl").
-include("parser.hrl").
-define(UA, "sensor-cloud:").

%% ====================================================================
%% API functions
%% ====================================================================
-export([poller_loop/3]).

%% @doc
%% Function: poller_loop/3
%% Purpose: the main loop of each poller to process the incoming messages.
%% @end
-spec poller_loop(integer(), string(), list()) ->none().
poller_loop(ResourceId, Url, ParsersList)->
	receive
		{rebuild}->
			%%to rebuild the poller 
			%%extract the url from the datastore according to resource id
			%%get the pasers from the datastore\
			%% TODO Check the available erlastic function calls
			Resource = erlastic:get_resource_by_id(ResourceId),  %% return a #resource record
			%% TODO Query erlastic for all parsers for resource with a specific resourceId (same as in pollingSystem)
			Parsers = parser:get_parsers_by_id(ResourceId), %% return a list of parsers [#parser, ...]
			NewUrl = Resource#resource.polling_url,
			%% notify the supervisor to refresh its records
			supervisor ! {update, ResourceId, NewUrl},
			poller_loop(ResourceId, NewUrl, Parsers);
		{probe}->
			%%communicate with external resources
			%%http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/
			application:start(inets),
			{ok, {{HttpVer, Code, Msg}, Headers, Body}} =
    			http:request(get, {Url, [{"User-Agent", (?UA++ResourceId)}]}, [], []),
			case Code==200 of
				true->
					case check_header(Headers) of
						"application/json" ->
							parser:applyParser(ParsersList, Body, "application/json");
						"no content type" ->
							%%error
							erlang:display("no content type is offered in the response");
						_ ->
							%%the other options
							erlang:display("other content type")
					end;
				_ ->
					%%polling fails
					erlang:display("polling failed")
			end,
			application:stop(inets),
			poller_loop(ResourceId, Url, ParsersList)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: check_header/1
%% Purpose: used to return the content-type of the response from the response`s header.
%% Returns: "no content type" | content-type
%% @end
-spec check_header(list()) -> string().
check_header([]) -> "no content type";
check_header([Tuple|Tail]) ->
	case Tuple of
		{"content-type", Res} -> Res;
		_ -> check_header(Tail)
	end.



