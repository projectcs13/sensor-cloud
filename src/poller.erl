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
-behaviour(gen_server).
-include("parser.hrl").
-include("state.hrl").
-include("common.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, init/1, handle_call/3, handle_info/2, terminate/2]).

%% @doc
%% Function: start_link/1
%% Purpose: start function used to start the poller, will call init/1 function later
%% Returns: {ok, Pid} | {error, ErrMsg}
%% @end
start_link(State)->
	gen_server:start_link(?MODULE, State, []).

%% @doc
%% Function: init/1
%% Purpose: init function used to initialize this poller gen_server.
%% Returns: {ok, State}
%% Side Effects: start inets and start the ssl
%% @end
init(State)->
	application:start(inets),
	ssl:start(),
	{ok, State}.

%% @doc
%% Function: handle_call/2
%% Purpose: handle synchronous call of gen_server, could be called via: gen_server:call(pid(), {rebuild})
%% Returns: {reply, (returned message), (new state of gen_server)}
%% @end
handle_call({rebuild}, _Form, State)->
	ResourceId = State#state.resourceid,
	Url = State#state.url,
	%%to rebuild the poller 
	%%extract the url from the datastore according to resource id
	%%get the pasers from the datastore

	%%I let each poller interact directly with the erlasticsearch module, not sure it is ok.
	case erlastic_search:get_doc(?ES_INDEX, "resource", ResourceId) of 
		{error,Reason} -> 
			erlang:display("Failed to retrieve the resource according to resource`s id"),
			erlang:display("The error reason: "++Reason),
			
			{reply, {error, Reason}, State};
		{ok,JsonStruct} ->
		    FinalJson = lib_json:get_and_add_id(JsonStruct),
			
			Parsers = poll_help:get_parsers_by_id(ResourceId), %% return a list of parsers [#parser, ...]
			case Parsers of
				{error, ErrMsg} ->
					erlang:display("parsers not found"),
					{reply, {error, ErrMsg}, State};
				_ ->
					continue
			end,
			
			NewUrl = lib_json:get_field(FinalJson, "url"),
			case is_binary(NewUrl) of
				false->
					FinalUrl = NewUrl;
				_ ->
					FinalUrl = binary_to_list(NewUrl)
			end,			
			%% notify the supervisor to refresh its records
			{reply, {update, ResourceId, FinalUrl}, #state{resourceid=ResourceId, url=FinalUrl, parserslist=Parsers}}
	end;
handle_call({check_info}, _Form, State)->
	%% return the information of poller
	{reply, {info, State}, State}.
	
%% @doc
%% Function: handle_info/2
%% Purpose: handle messages processing of the gen_server, could be called via: pid()!{probe}
%% Returns: {noreply, NewState}
%% @end
handle_info({probe}, State)->
	ResourceId = State#state.resourceid,
	ParsersList = State#state.parserslist,
	Url = State#state.url,
	
	%%communicate with external resources
	%%http://userprimary.net/posts/2009/04/04/exploring-erlangs-http-client/
	
	case httpc:request(get, {Url, [{"User-Agent", (?UA++ResourceId)}]}, [], []) of
		{ok, {{HttpVer, Code, Msg}, Headers, Body}}->
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
			{noreply, State};
		{error, Reason}->
			erlang:display("failed to poll external resource, "++Reason),
			{noreply, State};
		_ ->
			erlang:display("failed to poll external resource"),
			{noreply, State}
	end.

terminate(_T, State)->
	Url = State#state.url,
	erlang:display("the poller for "++Url++" stops working!"),
	application:stop(inets).

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



