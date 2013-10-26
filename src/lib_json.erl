%% @author Tommy Mattsson, Georgios Koutsoumpakis [www.csproj13.student.it.uu.se]
%% @copyright [Copyright information]
%% @version 1.0
%% @doc == Library for creating, reading, updating, and deleting fields in JSON objects ==
%% @end
-module(lib_json).
-include("erlson.hrl").
%% ====================================================================
%% API functions - Exports
%% ====================================================================
-export([add_field/3,
	 add_value/3,
	 add_value_in_list/2,
	 decode/1, 
	 encode/1, 
	 field_value_exists/3, 
	 get_field/2, 
	 get_fields/2,
	 get_field_value/3, 
	 replace_field/3,
	 rm_field/2,
	 set_attr/2,
	 to_string/1]).
%% ====================================================================
%% Specialized functions - Exports
%% ====================================================================
-export([get_and_add_id/1, 
	 get_list_and_add_id/1
	]).
-include("misc.hrl").
%% ====================================================================
%% Type definitions
%% ====================================================================
%% @type attr() = atom() | string()
-type attr() :: atom() | string().
%% @type field() = json_string() | mochijson()
-type field() :: atom() | string() | [atom()] .
%% @type json() = json_string() | mochijson()
-type json() :: json_string() | mochijson().
%% @type json_string() = string()
-type json_string() :: string().
%% @type json_value() = atom() | binary() | integer() | string() | json()
-type json_value() :: atom() | binary() | integer() | string() | json() | [json()].
%% @type mochijson() = tuple() 
-type mochijson() :: tuple(). 


%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 
%% Adds a new field with the name 'Field' and the value 'Value' to a JSON object.
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Field = "attr2".
%% > Value = "value2".
%% > lib_json:add_field(Json, Field, Value).
%% "{\"attr1\":\"value1\", \"attr2\":\"value2\"}"
%% '''
%% @end
-spec add_field(Json::json(),Field::field(),Value::json_value()) -> json_string().
add_field(Json, Field, Value) ->
    add_field_internal(prepare_json(Json), Field, prepare_value(Value)).

%% @doc 
%% TODO Should be improved to a general add_value(Json, Query, Value) function
%% @end 
-spec add_value_in_list(List::list(),Value::json_value()) -> list().
add_value_in_list(List, Value) when is_list(List) ->
	erlang:display(List),
	V = decode(Value),
	erlang:display(V),
	A= {struct,[{<<"streams">>, [V | List]}]},
	erlang:display("AAA"),
	erlang:display(to_string(A)),
	A.

%% @doc 
%% TODO Should be improved to a general add_value(Json, Query, Value) function
%% @end 
-spec add_value(Json::json(), Field::field(),Value::json_value()) -> json_string().
add_value(Json, Field, Value)  ->
    add_value_internal(prepare_json(Json), Field, prepare_value(Value)).

%% @doc 
%% Decodes a json object into mochijson format.
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > lib_json:decode(Json).
%% {struct,[{<<"attr1">>,<<"value1">>}]}
%% '''
%% @end
-spec decode(Json::json_string()) -> mochijson().
decode(Json) when is_list(Json) ->
     mochijson2:decode(Json).

%% @doc
%% Encodes a json object into an iolist().
%%
%% Example:
%% ```
%% > Json = {struct,[{<<"attr1">>,<<"value1">>}]}".
%% > lib_json:encode(Json).
%% [123,[34,<<"attr1">>,34],58,[34,<<"value1">>,34],125]
%% '''
%% @end
-spec encode(Json::json()) -> iolist().
encode(Json) when is_tuple(Json)->
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(Json);
encode(Json) when is_list(Json) ->
    JsonObj = decode(Json),
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(JsonObj).

%% @doc
%% Check if a specific field with a specific value exists in a JSON object.
%% Handles wildcard searches for fields (not wildcard for values.
%% 
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Query = "attr1".
%% > lib_json:field_value_exists(Json, Query, Value).
%% true
%% '''
%% @end
-spec field_value_exists(Json::json(), Query::string(), Value::json_value()) -> boolean().
field_value_exists(Json, Query, Value) ->
    case get_field_value(Json, Query, Value) of
	Value ->
	    true;
	undefined ->
	    false
    end.

%% @doc
%% Get the value at a certain field in a JSON object.
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Query = "attr1".
%% > lib_json:get_field(Json, Query).
%% "value1"
%% '''
%% @end
-spec get_field(Json::json(), Query::string()) -> json_value().
get_field(Json, Query) when is_tuple(Json) ->
    get_field_internal(Json, Query);
get_field(Json, Query) when is_list(Json)->
    JsonObj = decode(Json),
    get_field_internal(JsonObj, Query).


%% @doc
%% Get the values for a list of specified fields.
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\",\"attr2\":\"value2\"}".
%% > Fields = ["attr1", "attr2"].
%% > lib_json:get_fields(Json, Fields).
%% ["value1", "value2"]
%% '''
%% @end
-spec get_fields(Json::json(), Fields::[field()]) -> [json_value()].
get_fields(Json, []) ->
    [];
get_fields(Json, [Field|Tl]) ->
    Result = get_field(Json, Field),
    [Result | get_fields(Json, Tl)].
    
%% @doc
%% Get a certain value of a certain field. Returns 'undefined' if there is no field with that value.
%%
%% Example:
%% ```
%% > Json = "{\"attr1\": [{\"attr2\":\"value1\"},{\"attr2\":\"value2\"}]}".
%% > Query = "attr1[*].attr2".
%% > Value = "value2".
%% > lib_json:get_field_value(Json, Query, Value).
%% "value2"
%% '''
%% @end
-spec get_field_value(Json::json(), Query::field(), Value::json_value()) -> json_string().
get_field_value(Json, Query, Value) when is_list(Value)->
    QueryParts = find_wildcard_fields(Query),
    SearchFor = case {hd(Value),lists:last(Value)} of
		 {$*,$*} -> {contains, lists:sublist(Value)};
		 {_ ,$*} -> {suffix,   Value};
		 {$*, _} -> {prefix,   Value};
		 _ ->  Value
	     end,
    Search = field_recursion(Json, QueryParts, SearchFor),
    case SearchFor of
	{contains, SearchVal} ->
	    Value;
	{suffix,   SearchVal} ->
	    Value;
	{prefix, Value} ->
	    %% case lists:prefix() of;
		Value;
	_ ->
	    case Search of
		Value ->
		    Value;
		improper_usage ->
		    improper_usage;
		_ ->
		    undefined
	    end
    end;
get_field_value(Json, Query, Value) ->
    QueryParts = find_wildcard_fields(Query),
    case field_recursion(Json, QueryParts, Value) of
	Value ->
	    Value;
	_ ->
	    undefined
    end.

%% @doc
%% Replaces the value of a field 'Query' with 'Value' in a JSON object
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Query = "attr1".
%% > Value = <<"poff">>.
%% > lib_json:replace_field(Json, Query, Value).
%% "{\"attr1\":\"poff\"}"
%% '''
%% @end
-spec replace_field(Json::json(), Query::field(), Value::json_value()) -> json_string().
replace_field(Json, Query, Value) ->
    replace_field_internal(prepare_json(Json), Query, prepare_value(Value)).

%% @doc
%% Removes the field 'Query' from a JSON object
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Query = "attr1".
%% > lib_json:rm_field(Json, Query).
%% "{}"
%% '''
%% @end
-spec rm_field(Json::json(), Query::field()) -> json_string().
rm_field(Json, Query)  ->
    rm_field_internal(prepare_json(Json), Query).

%% @doc
%% Sets a json attribute 'Attr' to 'Value'
%%
%% Example:
%% ```
%% > Json = "{\"attr1\":\"value1\"}".
%% > Attr = "attr1".
%% > Value = value
%% > lib_json:set_attr(Attr, Value).
%% "{\"attr1\":\"value\"}"
%% '''
%% @end
-spec set_attr(Attr::attr(), Value::json_value()) -> json_string().
set_attr(Attr, Value) when is_atom(Attr) ->
    set_attr(binary:list_to_bin(atom_to_list(Attr)), Value);
set_attr(Attr, Value) when is_list(Attr) ->
    set_attr(binary:list_to_bin(Attr), Value);
set_attr(Attr, Value) when is_binary(Attr) ->
    to_string({struct, [{Attr, Value}]}).



%% @doc
%% Converts a mochijson structure or a mochijson encoded structure into a string
%%
%% Example:
%% ```
%% > Json = {struct,[{<<"attr1">>,<<"value1">>}]}.
%% > lib_json:to_string(Json).
%% "{\"attr1\":\"value\"}"
%% '''
%% @end
-spec to_string(Json::mochijson() | iolist()) -> json_string().
to_string(Json) when is_tuple(Json) ->
    to_string(encode(Json));
to_string(Json) when is_binary(Json) ->
    binary:bin_to_list(Json);
to_string(Json) ->
    %% Flattens a list and converts the entire thing into a binary.
    BinaryJson = binary:list_to_bin(Json),
    %% Converts the binary into a string.
    binary:bin_to_list(BinaryJson).


%% ====================================================================
%% Specialized functions
%% ====================================================================
%% @doc 
%% Gets the '_id' from the root, gets the '_source'. Adds _id as 
%% id' in _source and return the new JSON object.
%%
%% TODO Move to api_help
%% @end 
-spec get_and_add_id(JsonStruct::mochijson()) -> json_string().
get_and_add_id(JsonStruct) ->
    Id  = get_field(JsonStruct, "_id"),
    SourceJson  = get_field(JsonStruct, "_source"),
    add_field(SourceJson, "id", Id).

%% @doc 
%% Get the search results and performs get_and_add_id/1 on each
%% elements in the result list.
%%
%% TODO Move to api_help
%% @end 
-spec get_list_and_add_id(JsonStruct::mochijson()) -> json_string().
get_list_and_add_id(JsonStruct) ->
    HitsList = get_field(JsonStruct, "hits.hits"),
    AddedId = lists:map(fun(X) -> decode(get_and_add_id(X)) end, HitsList),
    HitsAttr = set_attr(hits, AddedId),
    to_string(HitsAttr).


%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc
%% @hidden
%% Function: add_field_internal/3
%% @end
add_field_internal(Json, Field, Value) ->
    Attrs = parse_attr(Field),
    
    try erlson:store(Attrs, Value, Json) of
	NewJson ->
	    to_string(erlson:to_json(NewJson))
    catch
	_:_ ->
	    to_string(erlson:to_json(Json))
    end.

%% @doc
%% @hidden
%% Function: add_value_internal/3
%% @end
add_value_internal(Json, Field, Value) ->
    Attrs = parse_attr(Field),    
    try erlson:store(Attrs, Value, Json) of
	NewJson ->
	    to_string(erlson:to_json(NewJson))
    catch
	_:_ ->
	    to_string(erlson:to_json(Json))
    end.

%% @doc
%% @hidden
%% Function: field_recursion/3
%% Purpose: Handles recursion over the specific fields in a json object 
%% Returns: Either the found value or 'false'
%% @end
field_recursion(Json, QueryParts, Value) ->
    field_recursion(Json, QueryParts, Value, "").

%% @doc
%% @hidden
%% Function: field_recursion/4
%% Purpose: Handles recursion over the specific fields in a json object 
%% Returns: Either the found value or 'false'
%% @end
field_recursion(Json, [{wildcard, Field} | Rest], Value, Query) ->
    case get_field_max_index(Json, Field) of
	N when is_integer(N) ->
	    case index_recursion(Json, [{wildcard, Field, N}| Rest], Value, Query) of
		Value ->
		    Value;
		R ->
		    R
	    end;
	R ->
	    R
    end;
field_recursion(Json, [{no_wildcard, Field}], Value, Query) ->
    NewQuery = lists:concat([Query, Field]),
    case get_field(Json, NewQuery) of
	R when is_list(R) ->
	    case lists:member(Value, lists:map(fun(X) when is_binary(X) -> binary:bin_to_list(X);
						  (X) -> X
					       end, R)) of
		true ->
		    Value;
		false ->
		    R
	    end;
	R ->
	    R
    end.

%% @doc
%% @hidden
%% Function: get_field/1
%% Purpose: Get the value at a certain field
%% Returns: Return the string representation of the value of the specified 
%%          field, if it exists, otherwise returns 'false' value. If the desired
%%          value is a struct then no adaptation is performed.
%% @end
-spec get_field_internal(Json::string() | tuple(), Query::string()) -> string() | atom().
get_field_internal(Json, Query) ->
    JsonParser = destructure_json:parse("Obj."++Query),
    try JsonParser(Json) of
	Result when is_binary(Result) ->
	    ?TO_STRING(Result);
	Result ->
	    Result
    catch
	error:function_clause -> undefined;
	error:{badfun, _} -> improper_usage
    end.


%% @doc
%% @hidden
%% Function: index_recursion/4
%% Purpose: Handles recursion over indexes for one specific field
%% Returns: Either the found value or 'false'
%% @end
index_recursion(Json, [{wildcard, Field, 0 = N} | Rest], Value, Query) ->
    NewQuery = lists:concat([Query, Field]),
    NewIndexQuery = query_index_prep(NewQuery, N),
    field_recursion(Json, Rest, Value, NewIndexQuery);
index_recursion(Json, [{wildcard, Field, N} | Rest], Value, Query) ->
    NewQuery = lists:concat([Query, Field]),
    NewIndexQuery = query_index_prep(NewQuery, N),
    case field_recursion(Json, Rest, Value, NewIndexQuery) of
	Value ->
	    Value;
	_ ->
	    index_recursion(Json, [{wildcard, Field, N-1} | Rest], Value, Query)
    end.
    
%% @doc
%% @hidden
%% Function: get_field_max_index/2
%% Purpose: Makes a query for a field in order to find out the amount of items for that field.
%%          Does NOT handle wildcard queries
%% Returns: The length of the item list or 'false' if the field was not found
%% @end    
get_field_max_index(Json, Query) ->
    case get_field(Json, Query) of
	R when is_list(R) ->
	    length(R) - 1;
	R ->
	    R
    end.

prepare_json(Json) when is_tuple(Json)->
    erlson:from_json(encode(Json));
prepare_json(Json) when is_list(Json)->
    erlson:from_json(Json).

prepare_value(Value) ->
    Value.


%% @doc
%% @hidden
%% Function: query_index_prep/2
%% Purpose: Encodes the index of a field for a json query
%% Returns: json query encoded as string()
%% @end
query_index_prep(Query, N) ->
    lists:concat([Query, "[", integer_to_list(N), "]"]).


%% @doc
%% @hidden
%% Function: find_wildcard_fields/1
%% Purpose:  Search the query for fields ending with [*]
%% Returns:  A list containing the different parts of the query. Each element is
%%           tagged as {wildcard, X} or {no_wildcard, X}
%% @end
find_wildcard_fields(Query) ->
    WildCards = re:split(Query, "\\[\\*\\]", [{return, list}]),
    case lists:last(WildCards) of
	[] ->
	    NewWildCards = lists:filter(fun(X) -> X =/= [] end, WildCards),
	    lists:map(fun(X) -> {wildcard, X} end, NewWildCards);
	R ->
	    NewWildCards = lists:sublist(WildCards, length(WildCards)-1),
	    NewWildCards2 = lists:map(fun(X) -> {wildcard, X} end, NewWildCards),
	    NewWildCards2 ++ [{no_wildcard, R}]
    end.

%% @doc
%% @hidden
%% Function: parse_attr/1
%% @end
parse_attr(Query) ->
    %% Produces a list such as ["attr1", attr2[1], attr3]
    Attrs = re:split(Query, "\\.", [{return, list}]),
    Fun = fun(X, Acc) ->
		  %% Produces a list such as ["attr1"] or ["attr2", "1]"]
		  case re:split(X, "\\[", [{return, list}]) of
		      [Attr] ->
			  %% The erlson library works on atoms
			  [list_to_atom(Attr)| Acc];
		      [Attr, IndexNoLeftBracket] ->
			  [Index, _] = re:split(IndexNoLeftBracket, "\\]", [{return, list}]),
			  %% The erlson library works on atoms and integers and the integers
			  %% cannot be 0, but the syntax specifies 0 as the first index,
			  %% so we add 1 to the index after conversion
			  [list_to_atom(Attr), (list_to_integer(Index)+1) | Acc] 
		  end
	  end,
    lists:foldr(Fun, [], Attrs).

%% @doc
%% @hidden
%% Function: replace_field_internal/3
%% @end
replace_field_internal(Json, Query, Value) ->
    Attrs = parse_attr(Query),
    %% Check if the value exist, if it doesn't exist it shouldn't be replaced
    case erlson:get_value(Attrs, Json) of
	undefined ->
	    to_string(erlson:to_json(Json));
	_ ->	   
	    try erlson:store(Attrs, Value, Json) of
		NewJson ->
		    to_string(erlson:to_json(NewJson))
	    catch
		_:_ ->
		    to_string(erlson:to_json(Json))
	    end
    end.

%% @doc
%% @hidden
%% Function: rm_field_internal/2
%% @end
rm_field_internal(Json, Query) ->
    Attrs = parse_attr(Query),
    try erlson:remove(Attrs, Json) of
	NewJson ->
	    to_string(erlson:to_json(NewJson))
    catch
	_:_ ->
	    to_string(erlson:to_json(Json))
    end.
