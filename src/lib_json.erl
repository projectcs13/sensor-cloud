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
%% Purpose : decodes a json object. 
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
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists,                                  
%%          otherwise returns the empty string.
%% To-Check:  what if there is a ',' inside a Value??? It will fail?
%% @end
-spec get_field(String::string(), Field::string()) -> string() | atom().
get_field(Json, Query) ->
    Result = get_field_help(Json, Query),
    mk_pretty(Result).

%% @doc
%% Function: get_field_/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists,                                  
%%          otherwise returns the empty string.
%% To-Check:  what if there is a ',' inside a Value??? It will fail?
%% @end
get_field_help(Json, Query) ->
    JsonObj = mochijson2:decode(Json),
    JsonParser = destructure_json:parse("Obj."++Query),
    JsonParser(JsonObj).



get_field_value(Json, Query, Value) ->
    QueryParts = find_wildcard_fields(Query),
    case field_recursion(Json, QueryParts, Value) of
	Value ->
	    Value;
	_ ->
	    false
    end.

field_value_exist(Json, Query, Value) ->
    case get_field_value(Json, Query, Value) of
	Value ->
	    true;
	false ->
	    false
    end.


mk_pretty(R) when is_binary(R) ->
    binary_to_list(R);

mk_pretty(R) when is_integer(R) ->
    integer_to_list(R);

mk_pretty(R) when is_list(R) ->
    lists:map(fun mk_pretty/1, R);

mk_pretty({struct, Values})  ->
    ValuesList = lists:map(fun tuple_to_list/1, Values),
    StringValues = lists:map(fun mk_pretty/1, ValuesList),
    lists:map(fun list_to_tuple/1, StringValues);

mk_pretty(R) ->
    R.

%% ====================================================================
%% Internal functions
%% ====================================================================
encode_help(Json) when is_tuple(Json)->
    JsonList = tuple_to_list(Json),
    encode_help(JsonList, "").

encode_help([], Acc) ->
    Acc;
encode_help([{Attr, Value} | Tl], Acc) when is_tuple(Value) ->
    Acc;
encode_help([{Attr, Value} | Tl], Acc) when is_list(Value) ->
    Acc.

    
    


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
    
    
get_field_max_index(Json, Query) ->
    case get_field_help(Json, Query) of
	R when is_list(R) ->
	    length(R) - 1;
	R ->
	    R
    end.

    
query_index_prep(Query, N) ->
    lists:concat([Query, "[", integer_to_list(N), "]"]).

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





%trims white spaces and quote from beggining and ending of the string
%% trim(String)->
%% 	Temp = re:replace(re:replace(String, "\\s+$", "", [global,{return,list}]), "^\\s+", "", [global,{return,list}]),
%% 	case re:run(Temp, "^{.*$}", [{capture, first, list}]) of
%% 		{match, _} -> 
%% 			A = Temp;
%% 		_->
%% 			case re:run(Temp, "$}", [{capture, first, list}]) of
%% 				{match, _} -> 
%% 					A = string:substr(Temp, 1, length(Temp)-1);
%% 				_->
%% 					A = Temp
%% 			end
%% 	end,
%% 	Temp = re:replace(re:replace(String, "\\s+$", "", [global,{return,list}]), "^\\s+", "", [global,{return,list}]),
%% 	string:strip(Temp, both, $").



%% @doc
%% Function: remove_special_characters/2
%% Purpose: Help function to remove non alphanumerical characters
%% Returns: First string of alphanumerical characters that can be found,
%%          empty string if non exists
%% @end
%% -spec remove_special_characters(String::string(),CharactersFound::boolean()) -> string().

%% remove_special_characters([],_) ->
%% 	[];

%% remove_special_characters([First|Rest],false) ->
%% 	Character = (First < $[) and (First > $@) or (First < ${) and (First > $`) or (First > $/) and (First < $:),
%% 	case Character of
%% 		true ->
%% 			[First|remove_special_characters(Rest,true)];
%% 		false ->
%% 			remove_special_characters(Rest,false)
%% 	end;
%% remove_special_characters([First|Rest],true) ->
%% 	Character = (First < $[) and (First > $@) or (First < ${) and (First > $`) or (First > $/) and (First < $:),
%% 	case Character of
%% 		true ->
%% 			[First|remove_special_characters(Rest,true)];
%% 		false ->
%% 			[]
%% 	end.


