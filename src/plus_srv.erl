%%%-------------------------------------------------------------------
%%% File:      plus_srv.erl
%%% @author    Ian Barber
%%% @copyright 2012 Google
%%% @doc  Example code on interacting with the discovery API
%%%
%%% @end
%%%
%%% @since 2012-11-05
%%%-------------------------------------------------------------------
-module(plus_srv).
-author('Ian Barber').
-behaviour(gen_server).

%% API
-export([start_link/4,list_apis/0,set_api/1,call_method/3,
         list_methods/0,list_scopes/1,gen_token_url/1,exchange_token/2,get_url/1]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {methods, baseurl, discovery, apikey, authtoken, oauth2state, clientid, clientsecret, redirecturl}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(APIKey, ClientID, ClientSecret, RedirectURL) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [APIKey, ClientID, ClientSecret, RedirectURL], []).


%%--------------------------------------------------------------------
%% @spec calL_method() -> {ok,Results} | {error,Error}
%% @doc Call a method and retrieve the resulting JSON document
%% @end
%%--------------------------------------------------------------------
call_method(Service, Parameters, Body)->
    gen_server:call(?MODULE,{call_method,Service,Parameters, Body}).

%%--------------------------------------------------------------------
%% @spec list_apis() -> {ok,Methods} | {error,Error}
%% @doc Returns a list of discoverable APIs
%% @end
%%--------------------------------------------------------------------
list_apis()->
    gen_server:call(?MODULE,{list_apis}).

%%--------------------------------------------------------------------
%% @spec set_api() -> {k | {error,Error}
%% @doc Set the server to operate against a specific API
%% @end
%%--------------------------------------------------------------------
set_api(ApiRestUrl)->
    gen_server:call(?MODULE,{set_api, ApiRestUrl}).

%%--------------------------------------------------------------------
%% @spec list_methods() -> {ok,Methods} | {error,Error}
%% @doc Returns a list of methods strings and aritys
%% @end
%%--------------------------------------------------------------------
list_methods()->
    gen_server:call(?MODULE,{list_methods}).

%%--------------------------------------------------------------------
%% @spec list_scopes() -> {ok,Scopes} | {error,Error}
%% @doc Returns a list of methods scopes. If empty, does not require auth
%% @end
%%--------------------------------------------------------------------
list_scopes(Method)->
    gen_server:call(?MODULE,{list_scopes,Method}).

%%--------------------------------------------------------------------
%% @spec gen_token_url() -> {ok,Url} | {error,Error}
%% @doc Returns a URL to pass to the user to authenticate the app
%% @end
%%--------------------------------------------------------------------
gen_token_url(Scopes)->
    gen_server:call(?MODULE,{gen_token_url,Scopes}).

%%--------------------------------------------------------------------
%% @spec exchange_token() -> ok | {error,Error}
%% @doc Returns a URL to pass to the user to authenticate the app
%% @end
%%--------------------------------------------------------------------
exchange_token(Code, State)->
    gen_server:call(?MODULE,{exchange_token,Code,State}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([APIKey, ClientID, ClientSecret, RedirectURL]) ->
    inets:start(),
    ssl:start(),
    {ok, #state{apikey=APIKey, clientid=ClientID, clientsecret=ClientSecret, redirecturl=RedirectURL,
                methods=[], baseurl=[], discovery=[], authtoken=[]}}.

%% Directory calls

handle_call({list_apis}, _From, State) ->
    [Apis, Discovery] = get_api_list(State#state.discovery),
    {reply, Apis, State#state{discovery=Discovery}};

handle_call({set_api, ApiRestUrl}, _From, State) ->
    [Methods, BaseURL] = get_api_methods(ApiRestUrl),
    {reply, ok, State#state{methods=Methods,baseurl=BaseURL}};

%% OAuth Calls

handle_call({gen_token_url, Scopes}, _From, State) ->
    [URL, OAuth2State] = get_authurl(Scopes, State),
    {reply, URL, State#state{oauth2state=OAuth2State}};

handle_call({exchange_token, Code, _}, _From, State) ->
% handle_call({exchange_token, Code, OAuth2State}, _From, State) when OAuth2State == State#state.oauth2state ->
    {ok, Token} = get_authtoken(Code, State),
    {reply, Token, State#state{authtoken=Token}};

handle_call({exchange_token, _, _}, _From, State) ->
    {reply, {error, "Invalid State"}, State};

%% Method calls
%% Error case for following calls - requires API to be set.
handle_call(_, _From, State) when length(State#state.methods)==0 ->
    {reply, {error, "No API Set"}, State};

handle_call({list_methods}, _From, State) ->
    {reply, get_method_names(State#state.methods), State};

handle_call({list_scopes, Method}, _From, State) ->
    {reply, get_method_scopes(Method, State#state.methods), State};

handle_call({call_method, Service, Parameters, Body}, _From, State) ->
    Reply = call_method(Service, Parameters, Body, State#state.methods, State),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% helper functions
%%====================================================================

get_url(Request) ->
    case Request of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}->
            {struct, Json} = mochijson2:decode(Body),
            {ok, Json};
        {ok, {{_Version, Code, _ReasonPhrase}, _Headers, _Body}}->
            {error,Code};
        {error,Error}->
            {error,Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            List APIs           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_api_list([]) ->
    case get_url(httpc:request(get,{"https://www.googleapis.com/discovery/v1/apis",[]},[],[])) of
        {error, Error} -> {stop, Error};
        {ok, Json} ->
            get_api_list(Json)
    end;

get_api_list(Discovery) ->
    Apis = proplists:get_value(<<"items">>, Discovery),
    [get_apis(Apis), Discovery].

get_api_methods(ApiRestUrl) ->
    case get_url(httpc:request(get,{ApiRestUrl,[]},[],[])) of
        {error, Error} -> {stop, Error};
        {ok, Json} ->
            Methods = get_resources(Json),
            BaseURL = binary_to_list(proplists:get_value(<<"baseUrl">>, Json)),
            [Methods, BaseURL]
    end.

get_apis([]) ->
    [];

get_apis([H|T]) ->
    {struct, API} = H,
    [{binary_to_list(proplists:get_value(<<"name">>, API)),
        binary_to_list(proplists:get_value(<<"discoveryRestUrl">>, API))}|get_apis(T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Call Methods         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_method(Service, Parameters, Body, [[Service, Method]|_], State) ->
    %% If we have an APIKey and don't need OAuth, include that
    BaseString = get_base_params(State#state.apikey, State#state.authtoken),
    Headers = get_headers(State),

    %% URL encode args,and paste on to end of string
    QS = lists:foldl(fun({Q,V}, QS) -> QS ++ "&" ++ Q ++ "=" ++ V end,
        BaseString, Parameters),
    %% Join method path on to uri
    Path = get_path(binary_to_list(proplists:get_value(<<"path">>, Method)), Parameters),
    Url = State#state.baseurl ++ Path ++ "?" ++ QS,

    %% erlang:display(Headers), %%% DEBUG %%%
    %% erlang:display(Url), %%% DEBUG %%%

    %% Switch on method
    [HttpMethod, Request] = case proplists:get_value(<<"httpMethod">>, Method) of
        <<"GET">> -> [get, {Url, Headers}];
        <<"POST">> -> [post, {Url, Headers, "application/json", Body}]
    end,

    %% Retrieve response
    %%[HttpMethod, Request];
    get_url(httpc:request(HttpMethod, Request, [], []));

call_method(Service, Parameters, Body, [_|Methods], State) ->
    call_method(Service, Parameters, Body, Methods, State).

get_headers(#state{authtoken=Token}) when 0 == length(Token) ->
    [];

get_headers(#state{authtoken=Token}) ->
    %% TODO: We should check the expiry of our authtoken!
    Auth = binary_to_list(proplists:get_value(<<"access_token">>, Token)),
    [{"Authorization", "Bearer " ++ Auth}].

get_base_params(APIKey, []) when APIKey /= [] ->
    "key=" ++ APIKey;

get_base_params(_, _) ->
    "".

get_path(Path, []) ->
    Path;

get_path(Path, [{Key, Value}|Parameters]) ->
    get_path(re:replace(Path, "{" ++ Key ++ "}", Value, [global, {return, list}]), Parameters).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieve Methods & Resources   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_method_scopes(Service, [[Service, Method]|_]) ->
    case proplists:get_value(<<"scopes">>, Method) of
        undefined -> [];
        Scopes -> Scopes
    end;

get_method_scopes(Service, [_|Methods]) ->
    get_method_scopes(Service, Methods).

get_method_names([[Method,_]|[]]) ->
    [Method];

get_method_names([[Method, _]|Methods]) ->
    [Method | get_method_names(Methods)].

get_resources(_, []) ->
    [];

get_resources(Json, [ResourceName|Resources]) ->
    {struct, Resource} = proplists:get_value(ResourceName, Json),
    lists:append(get_resources(Resource), get_resources(Json, Resources)).

get_resources(Json) ->
    %% If methods array defined, pull out list
    case proplists:get_value(<<"methods">>, Json) of
        {struct, Methods} ->
            get_methods(Methods, proplists:get_keys(Methods));
        undefined  ->
    %% If resource array defined, pull out and iterate over children
            {struct, ResourceTypes} = proplists:get_value(<<"resources">>, Json),
            get_resources(ResourceTypes, proplists:get_keys(ResourceTypes))
    end.

get_methods(_, []) ->
    [];

get_methods(Json, [MethodName|Methods]) ->
    {struct, Method} = proplists:get_value(MethodName, Json),
    [[binary_to_list(proplists:get_value(<<"id">>, Method)), Method] | get_methods(Json, Methods)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            OAuth               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_authurl(Scopes, #state{clientid = ClientID, redirecturl = RedirectURL}) ->
    OAuth2State = base64:encode_to_string(crypto:rand_bytes(12)),
    ["https://accounts.google.com/o/oauth2/auth?" ++
        "client_id=" ++ ClientID ++
        "&response_type=code" ++
        "&access_type=offline" ++
        % "&approval_prompt=force" ++
        "&redirect_uri=" ++ edoc_lib:escape_uri(RedirectURL) ++
        "&scope=" ++ edoc_lib:escape_uri(Scopes) ++
        "&state=" ++ edoc_lib:escape_uri(OAuth2State), OAuth2State].

get_authtoken(Code, #state{clientid = ClientID, clientsecret=ClientSecret, redirecturl = RedirectURL}) ->
    %% We check the OAuth2 state variable in the handler for security
    URL = "https://accounts.google.com/o/oauth2/token",
    Data =  "code=" ++ edoc_lib:escape_uri(Code) ++
            "&client_id=" ++ edoc_lib:escape_uri(ClientID) ++
            "&client_secret=" ++ edoc_lib:escape_uri(ClientSecret) ++
            "&redirect_uri=" ++ edoc_lib:escape_uri(RedirectURL) ++
            "&grant_type=authorization_code",
    get_url(httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], [])).
