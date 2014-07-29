%% @author Georgios Koutsoumpakis, Li Hao
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc Webmachine_resource for /users

-module(openidc).
-export([init/1,
         auth_gen_token/2,
         authenticate_token/2,
         exchange_token/2,
         fetch_user_info/0,
         process_auth_request/2,
         process_auth_redirect/2]).

% -include("webmachine.hrl").
% -include_lib("erlastic_search.hrl").
% -include("field_restrictions.hrl").

% TODO Grab these settings from a config file
-define(APIKEY, "AIzaSyCyC23vutanlgth_1INqQdZsv6AgZRiknY").
-define(CLIENT_ID, "995342763478-fh8bd2u58n1tl98nmec5jrd76dkbeksq.apps.googleusercontent.com").
-define(CLIENT_SECRET, "fVpjWngIEny9VTf3ZPZr8Sh6").
-define(REDIRECT_URL, "http://localhost:8000/users/_openid").

% %% @doc
% %% Function: init/1
% %% Purpose: init function used to fetch path information from webmachine dispatcher.
% %% Returns: {ok, undefined}
% %% @end
-spec init([]) -> {ok, undefined}.
init([]) ->
    {ok, undefined}.


-spec process_auth_request(ReqData::tuple(), State::string()) -> string().
process_auth_request(ReqData, State) ->
    plus_srv:start_link(?APIKEY, ?CLIENT_ID, ?CLIENT_SECRET, ?REDIRECT_URL),
    plus_srv:set_api("https://www.googleapis.com/discovery/v1/apis/plus/v1/rest"),
    plus_srv:gen_token_url("https://www.googleapis.com/auth/plus.me").


-spec process_auth_redirect(ReqData::tuple(), State::string()) -> tuple().
process_auth_redirect(ReqData, State) ->
    case {wrq:get_qs_value("code", ReqData), wrq:get_qs_value("state", ReqData)} of
        {undefined, _} -> {error, "State missing"};
        {_, undefined} -> {error, "Code missing"};

        {Code, AuthState} when Code =/= "", AuthState =/= "" ->
            case auth_gen_token(Code, AuthState) of
                {true, Res}    -> {ok, Res};
                {false, Error} -> {error, Error}
            end;

        _ -> {error, "Unsupported field(s) on the auth request"}
    end.


-spec auth_gen_token(Code::string(), AuthState::string()) -> string().
auth_gen_token(Code, AuthState) ->
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

-spec authorize_priviledge_request(ReqData::tuple()) -> tuple().
authorize_priviledge_request(ReqData) ->
    case authenticate_token("refresh_token", ReqData) of
        {error, Msg} -> {error, Msg};
        {ok, Token}  ->
            case wrq:get_req_header("Username", ReqData) of
                undefined -> {error, "Not possible to perform the request. Missing 'Username'"};
                Username when Username == "107908217220817548513" -> {ok, Token}
            end
    end.


% -spec is_priviledge(Username::string()) -> boolean().
% is_priviledge(Username) ->
%     Username == "107908217220817548513".
%     % Username == "107908217220817548513" or % Frontend
%     % Username == "107908217220817548513" or % Pub/Sub
%     % Username == "107908217220817548513".   % Polling-System


-spec fetch_user_info() -> tuple().
fetch_user_info() ->
    plus_srv:call_method("plus.people.get", [{"userId", "me"}], []).


-spec exchange_token(Code::string(), AuthState::string()) -> string().
exchange_token(Code, AuthState) ->
    Token = plus_srv:exchange_token(Code, AuthState),
    AccToken = proplists:get_value(<<"access_token">>, Token),
    RefToken = proplists:get_value(<<"refresh_token">>, Token),
    {AccToken, RefToken}.


-spec authenticate_request(ReqData::tuple()) -> tuple().
authenticate_request(ReqData) -> authenticate_token("access_token", ReqData).


-spec authenticate_token(TokenName::string(), ReqData::tuple()) -> tuple().
authenticate_token(TokenName, ReqData) ->
    case wrq:get_req_header(TokenName, ReqData) of
        undefined -> {error, "Not possible to perform the request. Missing " ++ TokenName};
        TokenVal  -> check_valid_token(TokenName, TokenVal)
    end.


%%% Private Functions

-spec check_valid_token(TokenName::string(), TokenValue::string()) -> tuple().
check_valid_token(TokenName, TokenValue) ->
    case verify_token(TokenValue) of
        {error, Msg} -> {error, Msg};
        {ok, JSON} ->
            case verify_token_response(TokenValue, JSON) of
                {error, Error}        -> {error, Error};
                {ok, Username, false} -> {error, "Delivered token not valid. Audience does not match the Client ID"};
                {ok, Username, true}  -> users:replace_access_token(Username, TokenValue)
            end
    end.


-spec verify_token(Token::string()) -> tuple().
verify_token(Token) ->
    AuthURL = "https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=" ++ Token,
    case plus_srv:get_url(httpc:request(get,{AuthURL,[]},[],[])) of
        {ok, Json} -> {ok, Json};
        {error, _} -> {error, "Delivered token already expired"}
    end.


-spec verify_token_response(NewToken::string(), JSON::tuple()) -> tuple().
verify_token_response(NewToken, JSON) ->
    case proplists:get_value(<<"error">>, JSON) of
        undefined ->
            Username = binary_to_list(proplists:get_value(<<"user_id">>, JSON)),
            case binary_to_list(proplists:get_value(<<"audience">>, JSON)) of
                undefined -> {error, "Delivered token not valid. Audience field not found"};
                Audience  -> {ok, Username, Audience == ?CLIENT_ID}
            end;
        Error -> {error, binary_to_list(Error)}
    end.

