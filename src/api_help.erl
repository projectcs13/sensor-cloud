%% @author Tomas Sävström <tosa7943@student.uu.se>, Li Hao <hali2222@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @headerfile "json.hrl"
%% @copyright [Copyright information]
%%
%% @doc == api_help ==
%% This module will contain all help functions used by the
%% api files
%%
%% @end
-module(api_help).
-include_lib("erlastic_search.hrl").
-include("json.hrl").
-include("field_restrictions.hrl").

-export([any_to_float/1,
	 do_any_field_exist/2,
	 do_only_fields_exist/2,
	 generate_date/1,
	 generate_error/2,
	 generate_timestamp/2,
	 get_elastic_search_url/0,
	 get_webmachine_url/0,
	 get_and_add_id/1,
	 get_info_request/1,
	 get_list_and_add_id/1,
	 get_list_and_add_id/2,
	 get_and_add_password/1,
	 get_list_and_add_password/1,
	 is_polling_history/1,
	 is_auth/1,
	 is_auth_redirect/1,
	 is_rank/1,
	 is_search/1,
	 is_count/1,
	 is_subs/1,
	 is_unsubs/1,
	 json_handler/2,
	 make_term_query/1,
	 now_to_seconds/0,
	 pair/1,
	 parse_path/1,
	 refresh/0,
	 transform/1,
	 transform/2,
	 update_doc/4,
	 update_doc/5
	]).

%% @doc
%% Purpose: Tries really hard to convert to float
%% Returns: float() or error
%%
%% @end
-spec any_to_float(any()) -> float().
any_to_float(Val) when is_float(Val) -> Val;
any_to_float(Val) when is_integer(Val) -> float(Val);
any_to_float(Val) when is_binary(Val) -> any_to_float(binary_to_list(Val));
any_to_float(".") -> error;
any_to_float(Val) when is_list(Val) ->
	case lists:prefix(".", Val) of
		true -> PointVal = "0"++Val;
		false -> PointVal = Val
	end,
	case lists:prefix("-.", PointVal) of
		true -> NewVal = "-0"++tl(PointVal);
		false -> NewVal = PointVal
	end,
    case string:to_float(NewVal) of
        {error,no_float} ->
        	try
        		case list_to_integer(NewVal) of
        			{error, no_integer} -> error;
        			V -> float(V)
        		end
        	catch error:badarg ->
        		error
        	end;
        {F,_Rest} -> F
    end;
any_to_float(Val) ->
	error.

%% @doc
%% Function: count_fields/1
%% Purpose: Used to return the number of fields in the given JSON
%% Returns: The number of fields in the given JSON
%% @end
-spec count_fields(Json::string()) -> integer().
count_fields(Json) ->
	count_fields(Json,1).

%% @doc
%% Function: count_fields/2
%% Purpose: Used to return the number of fields in the given JSON
%% Returns: The number of fields in the given JSON
%% @end
-spec count_fields(Json::string(),Position::integer()) -> integer().
count_fields(Json,Pos) ->
	case Pos == length(Json) of
		true -> 0;
		false ->
			case string:substr(Json, Pos, 2) of
				"\":" ->
					1 + count_fields(Json,Pos+1);
				_ ->
					case string:substr(Json, Pos, 3) of
						"\" :" ->
							1 + count_fields(Json,Pos+1);
						_ ->
							count_fields(Json,Pos+1)
					end
			end
	end.

%% @doc
%% Function: do_any_field_exist/2
%% Purpose: Used to check if a JSON contains any of the given fields
%% Returns: True if at least one of the given fields exist, false otherwise
%% @end
-spec do_any_field_exist(Json::string(),FieldList::list()) -> boolean().

do_any_field_exist(_Json,[]) ->
		false;
do_any_field_exist(Json,[First|Rest]) ->
		case lib_json:get_field(Json, First) of
			undefined ->
				do_any_field_exist(Json,Rest);
			_ ->
				true
		end.


%% @doc
%% Function: do_only_fields_exist/2
%% Purpose: Used to check if a JSON contains only fields given in the list
%% Returns: True if all fields are in the list, false otherwise
%% @end
-spec do_only_fields_exist(Json::string(),FieldList::list()) -> boolean().

do_only_fields_exist(Json,List) ->
	NumFields = count_fields(lib_json:to_string(Json)),
	Values = lib_json:get_fields(Json, List),
	NumCorrectFields = lists:foldl(fun(X, Sum) -> case X of
							  undefined -> Sum;
							  _ -> Sum + 1
						      end
				       end, 0, Values),
	NumFields == NumCorrectFields.

%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_date(DateList::list()) -> string().
generate_date([First]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First);
		false -> "" ++ integer_to_list(First)
	end;
generate_date([First|Rest]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest);
		false -> "" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest)
	end.

%% @doc
%% Purpose: generate an appropriate error string depending on the error code
%%
%% TODO: parse Body for more accurate response text
%%
%% @end
-spec generate_error(JSONString::string(), integer()) -> string().
generate_error(Body, ErrorCode) ->
	ErrorString = integer_to_list(ErrorCode),
	case ErrorCode of
		404 -> Reason = "not found";
		_ -> Reason = binary_to_list(Body)
	end,
	"Status " ++ ErrorString ++ "\nError: " ++ Reason.

%% @doc
%% Function: generate_timpestamp/2
%% Purpose: Used to create a timestamp valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day,Hour,Minute,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_timestamp(DateList::list(),Count::integer()) -> string().
generate_timestamp([],_) ->
	".000";
generate_timestamp([First|Rest],0) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ generate_timestamp(Rest,1);
		false -> "" ++ integer_to_list(First) ++ generate_timestamp(Rest,1)
	end;
generate_timestamp([First|Rest],3) ->
	case First < 10 of
		true -> "T0" ++ integer_to_list(First) ++ generate_timestamp(Rest,4);
		false -> "T" ++ integer_to_list(First) ++ generate_timestamp(Rest,4)
	end;
generate_timestamp([First|Rest],Count) when Count>3 ->
	case First < 10 of
		true -> ":0" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1);
		false -> ":" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1)
	end;
generate_timestamp([First|Rest],Count) ->
	case First < 10 of
		true -> "-0" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1);
		false -> "-" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1)
	end.


%% @doc
%% Returns the URL of Elastic Search
%%
%% @end
-spec get_elastic_search_url() -> string().
get_elastic_search_url() ->
	Port = case application:get_env(engine, es_port) of
		{ok, Value} ->
			Value;
		undefined ->
			9200
	end,
	Ip = case application:get_env(engine, es_ip) of
		{ok, Value2} ->
			Value2;
		undefined ->
			"localhost"
	end,
	"http://"++Ip++":"++integer_to_list(Port).

%% @doc
%% Returns the URL of webmachine
%%
%% @end
-spec get_webmachine_url() -> string().
get_webmachine_url() ->
	case application:get_env(engine, webmachine_port) of
		{ok, Value} ->
			"http://localhost:"++integer_to_list(Value);
		undefined ->
			"http://localhost:8000"
	end.

%% @doc
%% Gets the '_id' from the root, gets the '_source'. Adds _id as
%% id' in _source and return the new JSON object.
%%
%% @end
-spec get_and_add_id(JsonStruct::mochijson()) -> json_output_value().
get_and_add_id(JsonStruct) ->
    Id  = lib_json:get_field(JsonStruct, "_id"),
    SourceJson  = lib_json:get_field(JsonStruct, "_source"),
    lib_json:add_value(SourceJson, "id", Id).

%% @doc
%% Get the search results and performs get_and_add_id/1 on each
%% elements in the result list.
%% NOTE: Deprecated, use get_list_and_add_id/2 instead
%%
%% @end
-spec get_list_and_add_id(JsonStruct::mochijson()) -> json_string().
get_list_and_add_id(JsonStruct) ->
    HitsList = lib_json:get_field(JsonStruct, "hits.hits"),
    AddedId = lists:map(fun(X) -> get_and_add_id(X) end, HitsList),
    lib_json:set_attr(hits, AddedId).

%% @doc
%% Get the search results and performs get_and_add_id/1 on each
%% elements in the result list. The resulting list will be returned
%% as a proper JSON object with the JsonKey as the attribute.
%%
%% Return: get_list_and_add_id(List, Attribute) -> "{\"Attribute\": List}"
%% @end
-spec get_list_and_add_id(JsonStruct::mochijson(), atom()) -> json_string().
get_list_and_add_id(JsonStruct, JsonKey) ->
    HitsList = lib_json:get_field(JsonStruct, "hits.hits"),
    AddedId = lists:map(fun(X) -> get_and_add_id(X) end, HitsList),
    lib_json:set_attr(JsonKey, AddedId).

%% @doc
%% Gets the 'password' from the root, gets the '_source'. Adds password as
%% password' in _source and return the new JSON object.
%%
%% @end
-spec get_and_add_password(JsonStruct::mochijson()) -> json_output_value().
get_and_add_password(JsonStruct) ->
    Id  = lib_json:get_field(JsonStruct, "fields.password"),
    SourceJson  = lib_json:get_field(JsonStruct, "_source"),
    lib_json:add_value(SourceJson, "password", Id).

%% @doc
%% Get the search results and performs get_and_add_password/1 on each
%% elements in the result list.
%%
%% @end
-spec get_list_and_add_password(JsonStruct::mochijson()) -> json_string().
get_list_and_add_password(JsonStruct) ->
    HitsList = lib_json:get_field(JsonStruct, "hits.hits"),
    AddedPassword = lists:map(fun(X) -> get_and_add_password(X) end, HitsList),
    lib_json:set_attr(users, AddedPassword).

%% @doc
%% Function: get_info_request/1
%% Purpose: Retrieves the id from the path.
%% Returns: Id
%% @end
-spec get_info_request(ReqData::tuple()) -> string().
get_info_request(ReqData) ->
    Fetch_username = fun(TableName, Id) ->
        case erlastic_search:get_doc(?INDEX, TableName, Id) of
            {error, _} -> undefined;
            {ok, JSON} ->
                UID = lib_json:get_field(JSON, "_source.user_id"),
                string:to_lower(binary_to_list(UID))
        end
    end,

	Method = wrq:method(ReqData),

    {Resource, UserRequested} = case api_help:parse_path(wrq:path(ReqData)) of
        [{"users"}]                             -> {     "users", undefined};
        [{"users", Id}]                         -> {     "users", string:to_lower(Id)};
        [{"users", Id}, {Reso, Sid}]            -> {        Reso, string:to_lower(Id)};
        [{"users", Id}, {Reso}]                 -> {        Reso, string:to_lower(Id)};
        [{"users", Id}, {_, Sid}, {"triggers"}] -> {  "triggers", string:to_lower(Id)};
        [{"users", Id}, _]                      -> {     "users", string:to_lower(Id)};
        [{"streams"}]                           -> {   "streams", undefined};
        [{"streams", Id}]                       -> {   "streams", Fetch_username("stream",  Id)};
        [{"streams", Id}, {"_rank"}]            -> {      "rank", Fetch_username("stream",  Id)};
        [{"streams", Id}, {"data"}]             -> {"datapoints", Fetch_username("stream",  Id)};
        [{"streams", Id}, {"data", _}]          -> {"datapoints", Fetch_username("stream",  Id)};
        [{"streams", Id}, _]                    -> {   "streams", Fetch_username("stream",  Id)};
        [{"vstreams"}]                          -> {  "vstreams", undefined};
        [{"vstreams", Id}]                      -> {  "vstreams", Fetch_username("vstream", Id)};
        [{"vstreams", Id}, {"data"}]            -> {"datapoints", Fetch_username("vstream", Id)};
        [{"vstreams", Id}, {"data", _}]         -> {"datapoints", Fetch_username("vstream", Id)};
        [{"vstreams", Id}, _]                   -> {  "vstreams", Fetch_username("vstream", Id)};
        [{"resources"}]                         -> { "resources", undefined};
        [{"resources", Id}]                     -> { "resources", Fetch_username("resource", Id)};
        % [error]                               -> {   undefined, undefined};
        _                                       -> {   undefined, undefined}
    end,

    {Method, Resource, UserRequested}.



%% @doc
%% Function: is_polling_history/1
%% Purpose: Used to deiced if the URI specify a polling_history
%% Returns: True if URI specify a pollinghistory, false otherwise
%% @end
-spec is_polling_history(ReqData::term()) -> boolean().
is_polling_history(ReqData) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	lists:member("pollinghistory",URIList).

	%% @doc
%% Function: is_rank/1
%% Purpose: Used to decide if the URI specify a rank request
%% Returns: True if URI specifies a rank request, false otherwise
%% @end
-spec is_rank(ReqData::term()) -> boolean().
is_rank(ReqData) ->
	case string:str(wrq:path(ReqData),"_rank") of
		0 -> false;
		_ -> true
	end.


%% @doc
%% Function: is_search/1
%% Purpose: Used to decide if the URI specify a search
%% Returns: True if URI specify a search, false otherwise
%% @end
-spec is_search(ReqData::term()) -> boolean().
is_search(ReqData) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	(string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search").


%% @doc
%% Function: is_auth/1
%% Purpose: Used to decide if the URI specify a search
%% Returns: True if URI specify a search, false otherwise
%% @end
-spec is_auth(ReqData::term()) -> boolean().
is_auth(ReqData) ->
	% true.
	URIList = string:tokens(wrq:path(ReqData), "/"),
	(string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_auth").


%% @doc
%% Function: is_auth_redirect/1
%% Purpose: Used to decide if the URI specify a search
%% Returns: True if URI specify a search, false otherwise
%% @end
-spec is_auth_redirect(ReqData::term()) -> boolean().
is_auth_redirect(ReqData) ->
	% true.
	URIList = string:tokens(wrq:path(ReqData), "/"),
	(string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_openid").


%% @doc
%% Function: is_count/1
%% Purpose: Used to decide if the URI specify a count
%% Returns: True if URI specify a count, false otherwise
%% @end
-spec is_count(ReqData::term()) -> boolean().
is_count(ReqData) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	(string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_count").


%% @doc
%% Function: is_subs/1
%% Purpose: Used to decide if the URI specify a subs request
%% Returns: True if URI specifies a subs request, false otherwise
%% @end
-spec is_subs(ReqData::term()) -> boolean().
is_subs(ReqData) ->
	case string:str(wrq:path(ReqData),"_subscribe") of
		0 -> false;
		_ -> true
	end.

%% @doc
%% Function: is_unsubs/1
%% Purpose: Used to decide if the URI specify a unsubs request
%% Returns: True if URI specifies a unsubs request, false otherwise
%% @end
-spec is_unsubs(ReqData::term()) -> boolean().
is_unsubs(ReqData) ->
	case string:str(wrq:path(ReqData),"_unsubscribe") of
		0 -> false;
		_ -> true
	end.

%% @doc
%% Function: json_handler/2
%% Purpose: Used to get the json object from the request
%% Returns: {Json,ReqData,State}
%% @end
-spec json_handler(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
json_handler(ReqData, State) ->
	Value = binary_to_list(wrq:req_body(ReqData)),
	%[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
	{Value, ReqData, State}.


%% @doc
%% Function: make_term_query/1
%% Purpose: Used to create a term query from the given query string
%%          assumes that the query string is in the key:value&key:value ...
%%          format
%% Returns: The string the gives the term query
%% @end
-spec make_term_query(QueryString::string()) -> list().
make_term_query(String) ->
	"{\"" ++ make_inner_term_query(String) ++ "\"}".

%% @doc
%% Function: make_inner_term_query/1
%% Purpose: Used to create the inner part of the
%%          term query json.
%% Returns: The string the gives the inner part of the term query
%% @end
-spec make_inner_term_query(QueryString::string()) -> list().
make_inner_term_query([]) ->
	[];
make_inner_term_query([First|Rest]) ->
	case First of
		$: ->
			"\":\"" ++ make_inner_term_query(Rest);
		$& ->
			"\",\"" ++ make_inner_term_query(Rest);
		_ ->
			[First|make_inner_term_query(Rest)]
	end.

%% @doc
%% Function: now_to_seconds/0
%% Purpose: Converts the built-in function now() into seconds
%% Returns: The current number of seconds from 1970
%% @end
-spec now_to_seconds() -> integer().
now_to_seconds() ->
	{Mega, Sec, _} = erlang:now(),
    (Mega * 1000000) + Sec.


%% @doc
%% Function: pair/1
%% Purpose: Used to create a new list of tuples where each
%%          2 elements are paired
%% Returns: The paired list
%% @end
-spec pair(PathList::list()) -> list().
pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].

%% @doc
%% Function: parse_path/1
%% Purpose: Used to parse the URI path
%% Returns: The parsed URI path as a list
%% @end
-spec parse_path(Path::file:name_all()) -> list().
parse_path(Path) ->
	[_|T] = filename:split(Path),
	pair(T).

%% @doc
%% Function: refresh/0
%% Purpose: Help function to find refresh the sensorcloud index
%% Returns: {ok/error, {{Version, Code, Reason}, Headers, Body}}
%% @end
refresh() ->
    httpc:request(post, {get_elastic_search_url()++"/sensorcloud/_refresh", [],"", ""}, [], []).


%% @doc
%% Function: transform/1
%% Purpose: Transforms the query into the proper query language
%% Returns: string()
%% @end
-spec transform(tuple()) -> {string()}.
transform([]) -> "";
transform([{Field,Value}|Rest]) when is_binary(Field) andalso is_binary(Value)->
	case Rest of
		[] -> binary_to_list(Field) ++ ":" ++ binary_to_list(Value) ++ transform(Rest);
		_ -> binary_to_list(Field) ++ ":" ++ binary_to_list(Value) ++ "&" ++ transform(Rest)
	end;
transform([{Field,Value}|Rest]) ->
	case Rest of
		[] -> Field ++ ":" ++ Value ++ transform(Rest);
		_ -> Field ++ ":" ++ Value ++ "&" ++ transform(Rest)
	end.


%% @doc
%% Function: transform/2
%% Purpose: Used to create the query for search, expects more fields
%% if AddAnd equal to true
%% Returns: The query string from given from the list
%% were the list will be {Field,Value} tuples
%% @end
-spec transform(QueryList::list(),AddAnd::boolean()) -> list().
transform([],true) -> "&";
transform([],false) -> "";
transform([{Field,Value}|Rest],AddAnd) ->
	case Rest of
		[] -> Field ++ ":" ++ Value ++ transform(Rest,AddAnd);
		_ -> Field ++ ":" ++ Value ++ "&" ++ transform(Rest,AddAnd)
	end.

%% @doc
%% Function: update_doc/4
%% Purpose: Used to update document in elastic search
%% Returns: JSON response from elastic search server
%% @end
% Taken from erlasticsearch and modified to not encode
update_doc(Index, Type, Id, Mochijson) ->
    update_doc(Index, Type, Id, Mochijson, []).

%% @doc
%% Function: update_doc/5
%% Purpose: Used to update document in elastic search
%% Returns: JSON response from elastic search server
%% @end
% Taken from erlasticsearch and modified to not encode
update_doc(Index, Type, Id, Json, Qs) ->
    Id1 = mochiweb_util:quote_plus(Id),
    ReqPath = Index ++ [$/ | Type] ++ [$/ | Id1] ++ "/_update?retry_on_conflict=5",
    erls_resource:post(#erls_params{}, ReqPath, [], Qs, Json, []).
