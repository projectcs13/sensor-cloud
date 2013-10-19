%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == scoring ==
%% 
%%  
%%
%% @end

-module(lib_json).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_field/2, get_field_value/3]).

%% @doc
%% Function: get_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists, 
%%          otherwise returns the empty string.
%% To-Check:  what if there is a ',' inside a Value??? It will fail?
%% @end
-spec get_field(String::string(),Field::string()) -> string().
get_field(JsonString, Field) ->
    JsonObj = mochijson2:decode(JsonString),
    JsonParser = destructure_json:parse("Obj."++Field),
    case JsonParser(JsonObj) of
	R when is_binary(R) ->
	    binary_to_list(R);
	R when is_integer(R) ->
	    integer_to_list(R);
	R when is_list(R) ->
	    %%      Fix so that this function does not trim away stuff, add another get_field_help
	    %%      function that takes care of the actual search and get_field actually makes the
	    %%      the result pretty
	    %%	    lists:map(fun recursive_binary_to_list/1, R);
	R when is_tuple(R)->
	    recursive_binary_to_list(R);
	R when is_atom(R)->
	    R
    end.

get_field_value(JsonString, Field, Value) ->
    case re:run(Field, ".*\\[\\*\\]", [{capture, first, list}]) of
	{match, [WildcardField]} ->
	    FieldPart1 = WildcardField -- "[*]",
	    FieldPart2 = Field -- WildcardField,
	    Length = length(get_field(JsonString, FieldPart1)),
	    poff(JsonString, {FieldPart1, FieldPart2}, Length-1, Value);
	nomatch ->
	    case get_field(JsonString, Field) of
		Value ->
		    Value;
		_ ->
		    not_found
	    end
    end.
    
poff(JsonString, {FieldPart1, FieldPart2}, 0, _Value) ->
    get_field(JsonString, FieldPart1 ++ "[0]" ++ FieldPart2);
poff(JsonString, {FieldPart1, FieldPart2}, N, Value) ->
    Query = FieldPart1 ++ "[" ++ integer_to_list(N) ++ "]" ++ FieldPart2,
    case get_field(JsonString, Query) of
	Value ->
	    Value;

	R when is_list(R) ->
	    Length = length(R),
	    case poff(JsonString, {Query, ""}, Length, Value) of
		Value ->
		    Value;
		_ ->
		    poff(JsonString, {FieldPart1, FieldPart2}, N-1, Value)
	    end;


	_ ->
	    poff(JsonString, {FieldPart1, FieldPart2}, N-1, Value)
    end.

    
    
    


%% exist_value_field(JsonString, Field, Value) ->
%%     JsonString.



recursive_binary_to_list(X) when is_binary(X) ->
    binary_to_list(X);
recursive_binary_to_list({struct, Values}) ->
    ValuesList = lists:map(fun tuple_to_list/1, Values),
    StringValues = lists:map(fun recursive_binary_to_list/1, ValuesList),
    lists:map(fun list_to_tuple/1, StringValues);
recursive_binary_to_list(X) when is_list(X)->
    lists:map(fun recursive_binary_to_list/1, X).
    
    




























test(JsonObj, Fields) ->
	Tokens = string:tokens(Fields, "."),
	case Tokens of
		[ Field, Rest ] -> 
			NewObj = proplists:get_value(<<Field>>, JsonObj),
			test(NewObj, Rest);
		[Field] -> 
			erlang:display(Field),
			{struct, A} = proplists:get_value(<<Field>>, JsonObj),
			A
	end.
	


%% 	Tokens = string:tokens(JSONString, ","),
%% 	ResourceField = find_field(Tokens,Field),
%% 	case ResourceField of 
%% 		[] -> "";
%% 		_ -> FieldValue = string:tokens(ResourceField,":"),
%% 			 trim(lists:nth(length(FieldValue),FieldValue))
%% 			 %remove_special_characters(lists:nth(length(FieldValue),FieldValue),false)
%% 	end.




























%% ====================================================================
%% Internal functions
%% ====================================================================

destructure(JS, JSON) ->
    F = json:parse(JS),
    F(JSON).


%% @doc
%% Function: find_field/2
%% Purpose: Help function to find the first string containing the given string
%%          in the list.
%% Returns: The first string containing the given string
%%          in the list, the empty string if non exists.
%% @end
-spec find_field(List::list(),Field::string()) -> string().

find_field([],_) ->
	[];

find_field([First|Rest],Field) ->
	case string:str(First,Field) of
		0 -> find_field(Rest,Field);
		_ -> First
	end.


%trims white spaces and quote from beggining and ending of the string
trim(String)->
	Temp = re:replace(re:replace(String, "\\s+$", "", [global,{return,list}]), "^\\s+", "", [global,{return,list}]),
	case re:run(Temp, "^{.*$}", [{capture, first, list}]) of
		{match, _} -> 
			A = Temp;
		_->
			case re:run(Temp, "$}", [{capture, first, list}]) of
				{match, _} -> 
					A = string:substr(Temp, 1, length(Temp)-1);
				_->
					A = Temp
			end
	end,
	Temp = re:replace(re:replace(String, "\\s+$", "", [global,{return,list}]), "^\\s+", "", [global,{return,list}]),
	string:strip(Temp, both, $").



%% @doc
%% Function: remove_special_characters/2
%% Purpose: Help function to remove non alphanumerical characters
%% Returns: First string of alphanumerical characters that can be found,
%%          empty string if non exists
%% @end
-spec remove_special_characters(String::string(),CharactersFound::boolean()) -> string().

remove_special_characters([],_) ->
	[];

remove_special_characters([First|Rest],false) ->
	Character = (First < $[) and (First > $@) or (First < ${) and (First > $`) or (First > $/) and (First < $:),
	case Character of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			remove_special_characters(Rest,false)
	end;
remove_special_characters([First|Rest],true) ->
	Character = (First < $[) and (First > $@) or (First < ${) and (First > $`) or (First > $/) and (First < $:),
	case Character of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			[]
	end.


