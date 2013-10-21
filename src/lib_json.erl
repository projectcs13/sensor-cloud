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
-export([encode/1, decode/1, get_field/2, get_field_value/3, field_value_exist/3]).
-include("misc.hrl").


%% @doc
%% Function: encode/1
%% Purpose : Encodes an json object/
%% Returns : Either a string() representation of the json object or a mochiweb
%%           representation of a json object: json_term()
%% @end
encode(Json) when is_list(Json) ->
    JsonObj = mochijson2:decode(Json),
    mochijson2:encode(JsonObj);
encode({non_mochi, Json}) when is_tuple(Json) ->
    encode_help(Json);
encode(Json) when is_tuple(Json)->
    mochijson2:encode(Json).

%% @doc
%% Function: decode/1
%% Purpose : Decodes a json object. 
%% Returns : Either a mochiweb, json_term(), representation of the json object
%%           or a more readable version which only contains lists, tuples and string
%% @end
decode(Json) when is_list(Json) ->
     mochijson2:decode(Json);
decode({pretty, Json}) when is_list(Json)->
     JsonObj = decode(Json),
     mk_pretty(JsonObj).
%% decode({pretty, Json}) when is_tuple(Json) ->
%%     mk_pretty(Json).

    
    
%% @doc
%% Function: get_field/2
%% Purpose: Get the value at a certain field
%% Returns: Return the string representation of the value of the specified 
%%          field, if it exists, otherwise returns 'false' value.
%% @end
-spec get_field(String::string(), Field::string()) -> string() | atom().
get_field(Json, Query) ->
    Result = get_field_help(Json, Query),
    mk_pretty(Result).

%% @doc
%% Function: get_field_help/2
%% Purpose: Get a value at a certain field.
%% Returns: Return the value of the specified field, if it exists, otherwise
%%          returns 'false' value.
%% @end
get_field_help(Json, Query) ->
    JsonObj = mochijson2:decode(Json),
    JsonParser = destructure_json:parse("Obj."++Query),
    JsonParser(JsonObj).

%% @doc
%% Function: get_field_value/3
%% Purpose: Get a certain value of a certain field
%% Returns: Returns the value of the specified field, if it exists, otherwise
%%          returns 'false' value. Handles  wildcard searches for fields (not 
%%          wildcard for values)
%% @end
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
field_value_exist(Json, Query, Value) ->
    case get_field_value(Json, Query, Value) of
	Value ->
	    true;
	undefined ->
	    false
    end.


%% @doc
%% Function: mk_pretty/1
%% Purpose:  Make a json representation more readable. The functions in the 
%%           mochijson2 module is hard to read because it is often binaries
%% Returns:  list, tuple and string representation of the json object
%% @end
mk_pretty(Json) when is_binary(Json) ->
    binary_to_list(Json);

mk_pretty(Json) when is_integer(Json) ->
    integer_to_list(Json);

mk_pretty(Json) when is_list(Json) ->
    lists:map(fun mk_pretty/1, Json);

mk_pretty({struct, Values})  ->
    ValuesList = lists:map(fun tuple_to_list/1, Values),
    StringValues = lists:map(fun mk_pretty/1, ValuesList),
    lists:map(fun list_to_tuple/1, StringValues);

mk_pretty(Json) ->
    Json.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc
%% Function: encode_help/1
%% Purpose: Helper function for making a string of a tuple and list 
%%          representation of a json object
%% Returns: string()
%% TODO: NOT COMPLETED YET
%% @end
encode_help(Json) when is_tuple(Json)->
    JsonList = tuple_to_list(Json),
    encode_help(JsonList, "").

encode_help([], Acc) ->
    Acc;
encode_help([{Attr, Value} | Tl], Acc) when is_tuple(Value) ->
    Acc;
encode_help([{Attr, Value} | Tl], Acc) when is_list(Value) ->
    Acc.

    
    

%% @doc
%% Function: field_recursion/3
%% Purpose: Handles recursion over the specific fields in a json object 
%% Returns: Either the found value or 'false'
%% @end
field_recursion(Json, QueryParts, Value) ->
    field_recursion(Json, QueryParts, Value, "").

field_recursion(Json, [{wildcard, Field} | Rest], Value, Query) ->
    %% erlang:display("2"++Query),
    case get_field_max_index(Json, Field) of
	N when is_integer(N) ->
	    case index_recursion(Json, [{wildcard, Field, N}| Rest], Value, Query) of
		Value ->
		    %% erlang:display("3"),
		    Value;
		R ->
		    %% erlang:display("4a"),
		    %% erlang:display(R),
		    %% erlang:display("4b"),
		    R
		    %field_recursion(Json, Rest, Value, Query)
	    end;
	R ->
	    R
    end;
field_recursion(Json, [{no_wildcard, Field}], Value, Query) ->
    NewQuery = lists:concat([Query, Field]),
    %% erlang:display("5"++NewQuery),
    case mk_pretty(get_field_help(Json, NewQuery)) of
	R when is_list(R) ->
	    case lists:member(Value, R) of
		true ->
		    %% erlang:display("6a"),
		    Value;
		false ->
		    %% erlang:display("6b"),
		    R
	    end;
	R ->
	    %% erlang:display("6c"++R),
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
    %% erlang:display("7a"++NewIndexQuery),
    field_recursion(Json, Rest, Value, NewIndexQuery);
index_recursion(Json, [{wildcard, Field, N} | Rest], Value, Query) ->
    NewQuery = lists:concat([Query, Field]),
    NewIndexQuery = query_index_prep(NewQuery, N),
    %% erlang:display("8"++NewIndexQuery),
    case field_recursion(Json, Rest, Value, NewIndexQuery) of
	Value ->
	    %% erlang:display("9"),
	    Value;
	_ ->
	    %% erlang:display("10"),
	    index_recursion(Json, [{wildcard, Field, N-1} | Rest], Value, Query)
    end.
    
%% @doc
%% Function: get_field_max_index/2
%% Purpose: Makes a query for a field in order to find out the amount of items for that field.
%%          Does NOT handle wildcard queries
%% Returns: The length of the item list or 'false' if the field was not found
%% @end    
get_field_max_index(Json, Query) ->
    case get_field_help(Json, Query) of
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

