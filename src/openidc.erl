%% @author Georgios Koutsoumpakis, Li Hao
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /users

-module(openidc).
-export([authenticate/2,
         authenticate_token/2,
         exchange_token/2,
         fetch_user_info/0,
         process_auth_request/2,
         process_auth_redirect/2]).

% -include("webmachine.hrl").
% -include_lib("erlastic_search.hrl").
% -include("field_restrictions.hrl").

% %% @doc
% %% Function: init/1
% %% Purpose: init function used to fetch path information from webmachine dispatcher.
% %% Returns: {ok, undefined}
% %% @end
% -spec init([]) -> {ok, undefined}.
% init([]) ->
%         {ok, undefined}.


-spec process_auth_request(ReqData::tuple(), State::string()) -> string().
process_auth_request(ReqData, State) ->
    % TODO Grab these settings from a config file
    APIKey = "AIzaSyCyC23vutanlgth_1INqQdZsv6AgZRiknY",
    ClientID = "995342763478-fh8bd2u58n1tl98nmec5jrd76dkbeksq.apps.googleusercontent.com",
    ClientSecret = "fVpjWngIEny9VTf3ZPZr8Sh6",
    RedirectURL = "http://localhost:8000/users/_openid",

    plus_srv:start_link(APIKey, ClientID, ClientSecret, RedirectURL),
    plus_srv:set_api("https://www.googleapis.com/discovery/v1/apis/plus/v1/rest"),
    plus_srv:gen_token_url("https://www.googleapis.com/auth/plus.me").


-spec process_auth_redirect(ReqData::tuple(), State::string()) -> tuple().
process_auth_redirect(ReqData, State) ->
    case {wrq:get_qs_value("code", ReqData), wrq:get_qs_value("state", ReqData)} of
        {undefined, _} -> {error, "State missing"};
        {_, undefined} -> {error, "Code missing"};

        {Code, AuthState} when Code =/= "", AuthState =/= "" ->
            case authenticate(Code, AuthState) of
                {true, Res}    -> {ok, Res};
                {false, Error} -> {error, Error}
            end;

        _ -> {error, "Unsupported field(s) on the auth request"}
    end.


-spec authenticate(Code::string(), AuthState::string()) -> string().
authenticate(Code, AuthState) ->
    {AccToken, RefToken} = exchange_token(Code, AuthState),
    RefreshT = list_to_binary(string:substr(binary_to_list(RefToken), 3)),

    case AccToken of
        undefined -> {false, "Not possible to authenticate. Missing Access Token"};
        _ ->
            case fetch_user_info() of
                {error, _} -> {false, "Not possible to authenticate. Unreachable user info"};

                {ok, UserData} ->
                    case users:user_is_new(UserData) of
                        false -> Status = users:replace_old_access_token(UserData, AccToken);
                        true  -> Status = users:store_user(UserData, AccToken, RefreshT)
                    end,

                    case Status of
                        {error, Msg} -> {error, Msg};
                        {ok, _} ->
                            Struct = {struct, [
                                {access_token, AccToken},
                                {refresh_token, RefreshT}
                            ]},
                            Res = mochijson2:encode(Struct),
                            {true, Res}
                    end
            end
    end.


-spec authenticate_token(TokenName::string(), ReqData::tuple()) -> tuple().
authenticate_token(TokenName, ReqData) ->
    case wrq:get_qs_value(TokenName, ReqData) of
        undefined ->
            case wrq:get_req_header(TokenName, ReqData) of
                undefined -> {error, "Not possible to perform the request. Missing " ++ TokenName};
                TokenVal  -> users:get_user_by_token(TokenName, TokenVal)
            end;
        TokenValue -> users:get_user_by_token(TokenName, TokenValue)
    end.


-spec fetch_user_info() -> tuple().
fetch_user_info() ->
    plus_srv:call_method("plus.people.get", [{"userId", "me"}], []).


-spec exchange_token(Code::string(), AuthState::string()) -> string().
exchange_token(Code, AuthState) ->
    Token = plus_srv:exchange_token(Code, AuthState),
    AccToken = proplists:get_value(<<"access_token">>, Token),
    RefToken = proplists:get_value(<<"refresh_token">>, Token),
    {AccToken, RefToken}.
