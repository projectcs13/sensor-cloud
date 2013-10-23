%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == api_help ==
%% This module will contain all help functions used by the
%% api files 
%%
%% @end

-module(api_help).


%% should export all
-compile(export_all).

-include_lib("erlastic_search.hrl").



%% @doc
%% Function: make_to_string/1
%% Purpose: Used to convert JSON with binary data left to string
%% Returns: Returns the string represented by the given list
%% @end

make_to_string([]) ->
	[];
make_to_string([First|Rest]) ->
	case is_list(First) of
		true -> make_to_string(First) ++ make_to_string(Rest);
		false ->
			case is_binary(First) of
				true -> binary:bin_to_list(First) ++ make_to_string(Rest);
				false -> [First] ++ make_to_string(Rest)
			end
	end.
%% @doc
%% Function: remove_search_part/3
%% Purpose: Used to remove the search header of a search JSON 
%% Returns: Returns the list of JSON objects return from the search
%% @end
-spec remove_search_part(JSONString::string(),FoundLeft::boolean(),OpenBrackets::integer()) -> string().

remove_search_part([],_,_) ->
	[];
remove_search_part([First|Rest],true,1) ->
	case First of
		$] ->
			[First];
		$[ ->
			[First|remove_search_part(Rest,true,2)];
		_ ->
			[First|remove_search_part(Rest,true,1)]
	end;
remove_search_part([First|Rest],true,Val) ->
  	case First of
		$] ->
			[First|remove_search_part(Rest,true,Val-1)];
		$[ ->
			[First|remove_search_part(Rest,true,Val+1)];
		_ ->
			[First|remove_search_part(Rest,true,Val)]
	end;
remove_search_part([First|Rest],false,Val) ->
	case First of
		$[ ->
			[First|remove_search_part(Rest,true,1)];
		_ ->
			remove_search_part(Rest,false,Val)
	end.

%% @doc
%% Function: is_search/1
%% Purpose: Used to deiced if the URI specify a search
%% Returns: True if URI specify a search, false otherwise
%% @end
-spec is_search(ReqData::term()) -> boolean().

is_search(ReqData) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	(string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search").

%% @doc
%% Function: json_handler/2
%% Purpose: Used to get the json object from the request
%% Returns: {Json,ReqData,State}
%% @end
-spec json_handler(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	{Value, ReqData, State}.

%% @doc
%% Function: create_update/1
%% Purpose: Used to create the update document sent to erlastic search
%% Returns: The update document to send to erlasticsearch
%% @end
-spec create_update(Stream::string()) -> string().

create_update(Stream) ->
	"{\n\"doc\" : " ++ Stream ++ "\n}".

%% @doc
%% Function: add_field/3
%% Purpose: Used to add a new field to the given string representation of
%%          of a JSON object, the field will be FieldName : FieldValue
%% Returns: The string representation of the JSON object with the new field
%% @end
-spec add_field(Stream::string(),FieldName::string(),FieldValue::term()) -> string().

add_field(Stream,FieldName,FieldValue) ->
	case is_integer(FieldValue) of
		true ->
			string:substr(Stream,1,length(Stream)-1) ++ ",\"" ++ FieldName ++ "\":" ++ FieldValue ++ "}";
		false ->
			string:substr(Stream,1,length(Stream)-1) ++ ",\"" ++ FieldName ++ "\":\"" ++ FieldValue ++ "\"}"
	end.
			

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
%% if AddAnd euqal to true
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
%% Function: json_encode/1
%% Purpose: Used to transform the given data to json
%% Returns: JSON that is created
%% @end

% Taken from erlasticsearch
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).

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
    ReqPath = Index ++ [$/ | Type] ++ [$/ | Id1] ++ "/_update",
    erls_resource:post(#erls_params{}, ReqPath, [], Qs, Json, []).


%% @doc
%% Function: get_value_field/2
%% Purpose: Return the value of a certain field in the given JSON string.
%% Returns: Return the value of the specified field, if it exists, 
%%          otherwise returns the empty string.
%% @end
-spec get_value_field(String::string(),Field::string()) -> string().

get_value_field(JSONString,Field) ->
	Tokens = string:tokens(JSONString, ","),
	ResourceField = find_field(Tokens,Field),
	case ResourceField of 
		[] -> "";
		_ -> FieldValue = string:tokens(ResourceField,":"),
			 remove_special_characters(lists:nth(length(FieldValue),FieldValue),false)
	end.

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
	case string:str(First,"\"" ++ Field ++ "\"") of
		0 -> find_field(Rest,Field);
		_ -> First
	end.


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
	Character1 = (First < 91) and (First > 64) or (First < 123) and (First > 96) or (First > 47) and (First < 58),
	Character2 = Character1 or (First == $-) or (First == $_),
	case Character2 of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			remove_special_characters(Rest,false)
	end;
remove_special_characters([First|Rest],true) ->
	Character1 = (First < 91) and (First > 64) or (First < 123) and (First > 96) or (First > 47) and (First < 58),
	Character2 = Character1 or (First == $-) or (First == $_),
	case Character2 of
		true ->
			[First|remove_special_characters(Rest,true)];
		false ->
			[]
	end.


%% @doc
%% Function: remove_extra_info/3
%% Purpose: Used to remove the extra info for documents
%% Returns: Returns the list of JSON objects return from the search
%% @end
-spec remove_extra_info(JSONString::string(),OpenBrackets::integer()) -> string().

remove_extra_info([],_) ->
        [];
remove_extra_info([First|Rest],0) ->
        case First of
                ${ ->
                        remove_extra_info(Rest,1);
                _ ->
                        [First|remove_extra_info(Rest,0)]
        end;
remove_extra_info([First|Rest],1) ->
        case First of
                $} ->
                        remove_extra_info(Rest,0);
                ${ ->
                        [First|remove_extra_info(Rest,2)];
                _ ->
                        remove_extra_info(Rest,1)
        end;
remove_extra_info([First|Rest],Val) ->
          case First of
                $} ->
                        [First|remove_extra_info(Rest,Val-1)];
                ${ ->
                        [First|remove_extra_info(Rest,Val+1)];
                _ ->
                        [First|remove_extra_info(Rest,Val)]
        end.


remove_extra_and_add_id([]) ->
	[];
remove_extra_and_add_id(Json) ->
	Id = get_value_field(Json,"_id"),
	case Id of
		[] -> [];
		_->
			NewJson = add_field(remove_extra_info(get_object(Json,0),0),"id",Id),
			case get_value_field(remove_object(Json,0),"_id") of
				[] ->
					NewJson ++ remove_extra_and_add_id(remove_object(Json,0));
				_ ->
					NewJson ++ "," ++ remove_extra_and_add_id(remove_object(Json,0))
			end
	end.

	
%% @doc
%% Function: get_object/2
%% Purpose: Used to return the first JSON of the list
%% Returns: Returns first JSON in the list
%% @end
-spec get_object(JSONString::string(),OpenBrackets::integer()) -> string().

get_object([],_) ->
	[];
get_object([First|Rest],0) ->
	case First of
		$, ->
			[];
		${ ->
			[First|get_object(Rest,1)];
		_ ->
			get_object(Rest,0)
	end;
get_object([First|Rest],Val) ->
	case First of
		${ ->
			[First|get_object(Rest,Val+1)];
		$} ->
			[First|get_object(Rest,Val-1)];
		_-> 
			[First|get_object(Rest,Val)]
	end.

%% @doc
%% Function: remove_object/2
%% Purpose: Used to remove one object from the list
%% Returns: Returns the list of JSON objects left
%% @end
-spec remove_object(JSONString::string(),OpenBrackets::integer()) -> string().

remove_object([],_) ->
	[];
remove_object([First|Rest],0) ->
	case First of
		$, ->
			Rest;
		${ ->
			remove_object(Rest,1);
		_ ->
			[First|remove_object(Rest,0)]
	end;
remove_object([First|Rest],Val) ->
	case First of
		${ ->
			remove_object(Rest,Val+1);
		$} ->
			remove_object(Rest,Val-1);
		_-> 
			remove_object(Rest,Val)
	end.

%% @doc
%% Function: convert_binary_to_string/1
%% Purpose: convert binary parts of the list to string
%% Returns: Returns the list with the binary parts converted to strings
%% @end
-spec convert_binary_to_string(JSONString::string()) -> string().

convert_binary_to_string([]) ->
	[];
convert_binary_to_string([First|Rest]) ->
	case is_binary(First) of
		true -> binary_to_list(First) ++ convert_binary_to_string(Rest);
		false -> case is_list(First) of
					 true -> convert_binary_to_string(First) ++ convert_binary_to_string(Rest);
					 false -> [First] ++ convert_binary_to_string(Rest)
				 end
	end.

%% @doc
%% Function: get_id_value/2
%% Purpose: Help function to find value of a field in the string
%% Returns: String with value of the field
%% @end
-spec get_id_value(String::string(),Field::string()) -> string().

get_id_value(String,Field) ->
	Location = string:str(String,Field),
	Start = Location + 3 + length(Field),
	RestOfString = string:substr(String, Start),
	NextComma = string:str(RestOfString,","),
	NextBracket = string:str(RestOfString,"}"),
	case (NextComma < NextBracket) and (NextComma =/= 0) of
		true ->
			string:substr(RestOfString, 1,NextComma-2);
		false ->
			string:substr(RestOfString, 1,NextBracket-2)
	end.
