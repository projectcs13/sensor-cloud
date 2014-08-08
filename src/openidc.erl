%% @author Alberto Blazquez <albl1900@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]

%% @doc == openidc ==
%% Authentication/Authorization module based upon the OpenID Connect protocol
%% It relies heavily on the Google Sign in API (the Identity Provider)
%%
%% @end

-module(openidc).
-export([init/1,
         auth_request/1,
         authenticate/2,
         authorize/2,
         generate_idp_token/2,
         generate_own_token/1,
         is_priviledge/1,
         process_auth_request/2,
         process_auth_redirect/2]).

-include("field_restrictions.hrl").

% TODO Grab these settings from a config file
-define(APIKEY, "AIzaSyCyC23vutanlgth_1INqQdZsv6AgZRiknY").
-define(CLIENT_ID, "995342763478-fh8bd2u58n1tl98nmec5jrd76dkbeksq.apps.googleusercontent.com").
-define(CLIENT_SECRET, "fVpjWngIEny9VTf3ZPZr8Sh6").
-define(REDIRECT_URL, "http://localhost:8000/users/_openid").

-define(FRONTEND_ID, "107908217220817548513").
-define(PUB_SUB_ID,  "<< add here ... >>").
-define(POLLING_ID,  "<< add here ... >>").


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
            case generate_idp_token(Code, AuthState) of
                {true, Res}    -> {ok, Res};
                {false, Error} -> {error, Error}
            end;

        _ -> {error, "Unsupported field(s) on the auth request"}
    end.


-spec generate_idp_token(Code::string(), AuthState::string()) -> string().
generate_idp_token(Code, AuthState) ->
    {AccToken, RefToken} = exchange_token(Code, AuthState),

    % RefToken = case RefreshT of
    %     undefined -> list_to_binary("undefined");
    %     % String    -> list_to_binary(string:substr(binary_to_list(String), 3))
    %     String -> list_to_binary(String)
    % end,

    case AccToken of
        undefined -> {false, "Not possible to authenticate. Missing Access Token"};
        _ ->
            case fetch_user_info() of
                {error, _} -> {false, "Not possible to authenticate. Unreachable user info"};
                {ok, UserData} ->
                    Username = binary_to_list(proplists:get_value(<<"id">>, UserData)),
                    Status = case users:user_is_new(Username) of
                        true  -> users:store_user(UserData, AccToken, RefToken);
                        false ->
                            users:replace_token(Username, "access_token", AccToken),
                            users:replace_token(Username, "refresh_token", RefToken)
                    end,

                    case Status of
                        {error, Msg} -> {error, Msg};
                        {ok, _} ->
                            Struct = {struct, [
                                {access_token, AccToken},
                                {refresh_token, RefToken}
                            ]},
                            Res = mochijson2:encode(Struct),
                            {true, Res}
                    end
            end
    end.

-spec auth_request(ReqData::tuple()) -> tuple().
auth_request(ReqData) ->
    erlang:display("Authenticate"),
    case authenticate("Access-Token", ReqData) of
        {error, Error} -> erlang:display("error"), {error, "{\"error\": \"" ++ Error ++ "\"}"};
        {ok, UserID}   -> erlang:display("pre"), authorize(ReqData, UserID)
    end.


%%% Private Functions

-spec authenticate(TokenName::string(), ReqData::tuple()) -> tuple().
authenticate(TokenName, ReqData) ->
    case wrq:get_req_header(TokenName, ReqData) of
        undefined -> {error, "Not possible to perform the request. Missing " ++ TokenName};
        TokenVal  -> check_valid_token(TokenName, TokenVal)
    end.


-spec authorize(ReqData::tuple(), UserID::string()) -> tuple().
authorize(ReqData, UserID) ->
    {Method, Resource, UserRequested} = api_help:get_info_request(ReqData),

    erlang:display("Authorising..."),
    erlang:display({Method, Resource, UserRequested}),
    erlang:display({"UserID", UserID}),

    ValidAccess = case UserRequested of
        undefined -> check_auth_rules_collections(Method, Resource);
        _         -> check_auth_rules_individuals(Method, Resource, UserRequested, UserID)
    end,

    erlang:display({is_priviledge(UserID), ValidAccess}),
    case is_priviledge(UserID) or ValidAccess of
        true  -> {ok, UserID};
        false -> {error, "{\"error\": \"User not authorized. Permission denied\"}"}
    end.


check_auth_rules_individuals(Method, Resource, UserRequested, UserID) ->
    erlang:display("INDIVIDUAL"),
    {ok, UserJSON} = users:get_user_by_name(UserRequested),
    Private = lib_json:get_field(UserJSON, "private"),

    erlang:display({"Private", Private}),

    case {UserRequested == UserID, Private} of
        {true, _} ->
            case {Method, Resource} of                      % Rule 1: Can MAKE anything with our own data
                {'GET', "datapoints"} -> true;              %         except for manipulating datapoints
                {    _, "datapoints"} -> false;
                {'PUT',       "rank"} -> true;              % Rule 4: Can ONLY PUT the ranking of other user's stream
                _                     -> true
            end;

        {false, true}  -> false;                            % Rule 2: Can NOT MAKE anything to private users

        {false, false} ->
            case {Method, Resource} of
                {'GET',    "users"} -> true;                % Rule 3: Can ONLY GET User/S/VS from other public users
                {'GET',  "streams"} -> true;
                {'GET', "vstreams"} -> true;

                {'PUT', "rank"} -> true;                    % Rule 4: Can ONLY PUT the ranking of other user's stream

                _ -> false                                  % Rule 5: Anything else is forbidden
            end
    end.


check_auth_rules_collections(Method, Resource) ->
    erlang:display("Collection!!"),
    % Rule 6: Only fetch a collection or create a new User, Stream or Virtual Stream is allowed
    % This rule is checked in cases when a GET or POST to /users, without a specific user id, is requested
    ValidMethods   = (Method == 'GET') or (Method == 'POST'),
    ValidResources = (Resource == "users") or (Resource == "streams") or (Resource == "vstreams"),
    ValidMethods and ValidResources.


-spec check_valid_token(TokenName::string(), TokenValue::string()) -> tuple().
check_valid_token(TokenName, TokenValue) ->
    case verify_idp_token(TokenValue) of
        {error, Msg} ->
            case verify_own_token(TokenValue) of
                {error, Error}      -> {error, Error};
                {ok, true, UserID}  -> {ok, UserID};
                {ok, false, UserID} -> renew_token_and_replace(UserID, TokenName, TokenValue)
            end;

        {ok, GoogleJSON} ->
            case analyse_token_response(TokenValue, GoogleJSON) of
                {error, Error}     -> {error, Error};
                {ok, false}        -> {error, "Token not valid. Expended by other system"};
                {ok, true, UserID} ->
                    case users:replace_token(UserID, TokenName, TokenValue) of
                        {error, Error} -> {error, Error};
                        {ok, UserJSON} -> {ok, UserID}
                    end
            end
    end.


-spec verify_idp_token(Token::string()) -> tuple().
verify_idp_token(Token) ->
    AuthURL = "https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=" ++ Token,
    case plus_srv:get_url(httpc:request(get,{AuthURL,[]},[],[])) of
        {ok, Json} -> {ok, Json};
        {error, _} -> {error, "Token not valid or already expired"}
    end.


-spec analyse_token_response(NewToken::string(), JSON::tuple()) -> tuple().
analyse_token_response(NewToken, JSON) ->
    case proplists:get_value(<<"error">>, JSON) of
        undefined ->
            GoogleUsername = binary_to_list(proplists:get_value(<<"user_id">>, JSON)),
            case binary_to_list(proplists:get_value(<<"audience">>, JSON)) of
                undefined -> {error, "Token not valid. Audience field not found"};
                Audience  ->
                    Valid = (Audience == ?CLIENT_ID),
                    {ok, Valid, GoogleUsername}
            end;
        Error -> {error, "Error verifying the token with Identity Provider"}
    end.


-spec is_priviledge(Username::string()) -> boolean().
is_priviledge(Username) ->
    (Username == ?FRONTEND_ID) or (Username == ?PUB_SUB_ID) or (Username == ?POLLING_ID).


-spec verify_own_token(AccToken::string()) -> boolean().
verify_own_token(AccToken) ->
    case look_up_token(AccToken) of
        {error, Error} -> {error, Error};
        {ok, JSON} ->
            UserID = binary_to_list(lib_json:get_field(JSON, "_source.user_id")),

            CurrentTS = api_help:now_to_seconds(),
            IssuedAt  = lib_json:get_field(JSON, "_source.issued_at"),
            ExpiresIn = lib_json:get_field(JSON, "_source.expires_in"),  % 3600 seconds usually

            Valid = (CurrentTS < (IssuedAt + ExpiresIn)),
            erlang:display(IssuedAt),
            erlang:display(CurrentTS),
            erlang:display(Valid),

            {ok, Valid, UserID}
    end.


-spec look_up_token(AccToken::string()) -> tuple().
look_up_token(AccToken) ->
    erlang:display("Looking up..."),
    erlang:display(AccToken),
    case erlastic_search:get_doc(?INDEX, "token", AccToken) of
        {error, _} -> {error, "Token not found on the database"};
        {ok, List} -> {ok, List}
    end.


% Replace the old token and generate a new valid token because
% it is the only way to authorize a valid non-OpenID user
-spec renew_token_and_replace(UserID::string(), TokenName::string(), OldAccToken::string()) -> tuple().
renew_token_and_replace(UserID, TokenName, OldAccToken) ->
    case generate_own_token(UserID) of
        {error, Msg} -> {error, Msg};
        {ok, NewAccToken, _} ->
            erlastic_search:delete_doc(?INDEX, "token", OldAccToken),
            NewToken = binary_to_list(NewAccToken),
            case users:replace_token(UserID, TokenName, NewToken) of
                {error, Error} -> {error, Error};
                {ok, _}        -> {ok, UserID}
            end
    end.


-spec generate_own_token(Username::string()) -> tuple().
generate_own_token(Username) ->
    AccToken = base64:encode(crypto:strong_rand_bytes(32)),

    TokenJSON = mochijson2:encode({struct, [
        {access_token, AccToken},
        {expires_in, 3600},
        {issued_at, api_help:now_to_seconds()},
        {refresh_token, <<"undefined">>},
        {user_id, Username}
    ]}),

    case erlastic_search:index_doc_with_id(?INDEX, "token", AccToken, TokenJSON) of
        {error, _} -> {error, "Not possible to store the generated token"};
        {ok, List} -> {ok, AccToken, TokenJSON}
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
