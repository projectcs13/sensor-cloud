%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == Library for accessing fields in JSON objects ==
%% 
%%  
%%
%% @end
-module(lib_json).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add_field/3, 
	 decode/1, 
	 encode/1, 
	 field_value_exists/3, 
	 get_field/2, 
	 get_field_value/3, 
	 set_attr/2,
	 to_string/1]).
-include("misc.hrl").


%% @doc
%% Function: encode/1
%% Purpose : Encodes an json object/
%% Returns : Either a string() representation of the json object or a mochiweb
%%           representation of a json object: json_term()
%% @end

%% This case is for when Json is a string representation of the json object
encode(Json) when is_list(Json) ->
    JsonObj = decode(Json),
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(JsonObj);

%% This case is for when Json is a internal mochijson representation of the json object
encode(Json) when is_tuple(Json)->
    Encoder = mochijson2:encoder([{utf8, true}]),
    Encoder(Json);
encode(Json)->
    mochijson2:encode(Json).


%% @doc
%% Function: decode/1
%% Purpose : Decodes a json object. 
%% Returns : Either a mochiweb, json_term(), representation of the json object
%%           or a more readable version which only contains lists, tuples and string
%% @end
decode(Json) when is_list(Json) ->
     mochijson2:decode(Json).


%% @doc
%% Function: get_field/1
%% Purpose: Get the value at a certain field
%% Returns: Return the string representation of the value of the specified 
%%          field, if it exists, otherwise returns 'false' value. If the desired
%%          value is a struct then no adaptation is performed.
%% @end
-spec get_field({Json::string() | tuple(), Query::string()}) -> string() | atom().
get_field({Json, Query})->
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
%% Function: get_field/2
%% Purpose: Get the value at a certain field
%% Returns: Return the string representation of the value of the specified 
%%          field, if it exists, otherwise returns 'false' value. If the desired
%%          value is a struct then no adaptation is performed.
%% @end
-spec get_field(Json::string() | tuple(), Query::string()) -> string() | atom().
get_field(Json, Query) when is_list(Json)->
    JsonObj = decode(Json),
    get_field({JsonObj, Query});
get_field(Json, Query) when is_tuple(Json) ->
    get_field({Json, Query}).


%% @doc
%% Function: get_field_value/3
%% Purpose: Get a certain value of a certain field
%% Returns: Returns the value of the specified field, if it exists, otherwise
%%          returns 'false' value. Handles  wildcard searches for fields (not 
%%          wildcard for values)
%% @end
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
%% Function: field_value_exists/3
%% Purpose: Check if a specific field with a specific value exists
%%          Handles wildcard searches for fields (not wildcard for values)
%% Returns: Return true if the value exists, otherwise false
%% @end
field_value_exists(Json, Query, Value) ->
    case get_field_value(Json, Query, Value) of
	Value ->
	    true;
	undefined ->
	    false
    end.


%% @doc
%% Function: to_string/1
%% Purpose:  Make a json representation into a string
%% Returns:  string()
%% @end
%% to_string(Json) when is_list(Json) ->
%%     Json.

to_string(Json) when is_tuple(Json) ->
    to_string(encode(Json));
to_string([Hd|Tl]) when is_binary(Hd) ->
    %% erlang:display(Hd),
    binary:bin_to_list(Hd) ++ to_string(Tl);
to_string([]) ->
	[];
to_string([Hd|Tl]) when is_list(Hd)->
    %% erlang:display(Hd),
    to_string(Hd) ++ to_string(Tl);
to_string([Hd|Tl]) when is_integer(Hd) ->
    %% erlang:display(Hd),
    [Hd] ++ to_string(Tl).

set_attr(Attr, Value) when is_atom(Attr) ->
    set_attr(binary:list_to_bin(atom_to_list(Attr)), Value);
set_attr(Attr, Value) when is_binary(Attr) ->
    {struct, [{Attr, Value}]};
set_attr(Attr, Value) when is_list(Attr) ->
    set_attr(binary:list_to_bin(Attr), Value).
    

%% @doc
%% Function: add_field/3
%% Purpose: Used to add a new field to the given string representation of
%%          of a JSON object, the field will be FieldName : FieldValue
%% Returns: The string representation of the JSON object with the new field
%% @end
-spec add_field(Stream::string(),FieldName::string(),FieldValue::term()) -> string().

add_field(Json,FieldName,FieldValue) when is_tuple(Json)->
    add_field(to_string(Json), FieldName, FieldValue);
add_field(Stream,FieldName,FieldValue) when is_integer(FieldValue) ->
    string:substr(Stream,1,length(Stream)-1) ++ ",\"" ++ FieldName ++ "\":" ++ FieldValue ++ "}";
add_field(Stream,FieldName,FieldValue) ->
    string:substr(Stream,1,length(Stream)-1) ++ ",\"" ++ FieldName ++ "\":\"" ++ FieldValue ++ "\"}".


%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc
%% Function: field_recursion/3
%% Purpose: Handles recursion over the specific fields in a json object 
%% Returns: Either the found value or 'false'
%% @end
field_recursion(Json, QueryParts, Value) ->
    field_recursion(Json, QueryParts, Value, "").

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

%% @doc
%% Function: query_index_prep/2
%% Purpose: Encodes the index of a field for a json query
%% Returns: json query encoded as string()
%% @end
query_index_prep(Query, N) ->
    lists:concat([Query, "[", integer_to_list(N), "]"]).


%% @doc
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
