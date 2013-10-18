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
-export([get_value_field/2]).

%% @doc
%% Function: get_value_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists, 
%%          otherwise returns the empty string.
%% To-Check:  what if there is a ',' inside a Value??? It will fail?
%% @end
-spec get_value_field(String::string(),Field::string()) -> string().

get_value_field(JsonString, Field) when is_list(JsonString) ->
    JsonObj = mochijson2:decode(JsonString),
    JsonParser = destructure_json:parse("Obj."++Field),
    JsonParser(JsonObj);

get_value_field(JsonObj, Field) when is_tuple(JsonObj) ->    
    JsonParser = destructure_json:parse("Obj."++Field),
    JsonParser(JsonObj).



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


