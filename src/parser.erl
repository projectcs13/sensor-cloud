%% @author Tholsgård Gabriel, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == parser ==
%% this module implements functionalities to extract values from the data body,
%% and stores them into datapoints. In the current design, we only care about the values in the key-value pairs,
%% not the keys. The parsers in the parseJson and parseText functions are regular expressions. for instance, /sensor/temp/value
%% indicates the json object: {'sensor': {'temp': {'value': 123}}}
%% @end

-module(parser).
-include("parser.hrl").

-define(INDEX, "sensorcloud").
%% ====================================================================
%% API functions
%% ====================================================================
-export([applyParser/3, getParsersById/1]).

%% @doc
%% Function: applyParser/3
%% Purpose: used relative parsers to extract the values we want from the data. In the current design, the datapoint 
%% is a list of values, we care more on the values than the keys. ParsersList contains the regular expressions used to 
%% parse the different data. 
%% Returns: ok | {error, ErrMsg}
%% @end
-spec applyParser(list(),any() ,string()) -> ok | {error, string()}.
applyParser(ParsersList, Data, ContentType) -> 
	Parsers = get_parsers_with_content_type(ContentType, ParsersList, []),
	case Parsers == [] of
		false -> 
				doParsing(ParsersList, Data, ContentType);
		_ -> %%the parsers for this type do not exist.
				{error, "the parsers for this type do not exist"}
	end.

%% @doc
%% Function: getParsersById/1
%% Purpose: get parsers list according to specific resource id 
%% Returns: {error, ErrMsg} | [#parser .....]
%% @end
getParsersById(ResourceId)->
	case erlastic_search:search_limit(?INDEX, "parser", "resourceid:" ++ ResourceId, 100) of
		{ok, Result} ->
			EncodedResult = lib_json:encode(Result),
			case re:run(EncodedResult, "\"max_score\":null", [{capture, first, list}]) of
				{match, _} -> {error, "parsers not found"};
				nomatch -> FinalJsonList = lib_json:get_field(lib_json:get_list_and_add_id(Result), "hits"),
					       FinalJsonList,
						   
						   %%transform this json list to record list
						   jsonToRecord(FinalJsonList, [])
			end;
		_ -> {error, "parsers not found"}
	end. 

%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: jsonToRecord/2
%% Purpose: transform the json list to parser record list 
%% Returns: list()
%% @end
jsonToRecord([], Res)->
	Res;
jsonToRecord([Item|Tail], Res)->
	Tmp = #parser{resource_id = lib_json:get_field(Item, "resourceid"),
				  stream_id = lib_json:get_field(Item, "streamid"),
				  input_parser = lib_json:get_field(Item, "inputparser"),
				  input_type = lib_json:get_field(Item, "inputtype")
				 },
	jsonToRecord(Tail, [Tmp|Res]).

%% @doc
%% Function: processParser/2
%% Purpose: transformat the format of the parser
%% Example: ["streams","temperature","value"] => "streams.temperature.value" 
%% Returns: string()
%% @end
processParser([Item|Tail], Res)->
	case Item=="/" of
		true->
			processParser(Tail, Res);
		_ ->
			case Tail of
				[]->
					Res++Item;
				_ ->
					processParser(Tail, Res++Item++".")
			end
	end.

%% @doc
%% Function: parseJson/2
%% Purpose: used to parse the json data, unfinished and needs to be updated.
%% Returns: ok | {error, ErrMsg}
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseJson(record(), any()) -> ok.
parseJson(Parser, Data) ->
	%% extract the wanted value from the json-data and store it in the DB
	%% return the status of the transaction, ok or {error, ErrMsg}

	%%extract the data from the coming data
	ResourceId = Parser#parser.resource_id,
	StreamId = Parser#parser.stream_id,
	InputType = Parser#parser.input_type,
	InputParser = Parser#parser.input_parser,
	
	ItemList = filename:split(InputParser),
	Query = processParser(ItemList, []),
	Res = lib_json:get_field(Data, Query),
	
	%%insert the data as a new datapoint into the database
	%%since we need more time to investigate how to handle timestamp
	%%so currently we only consider the time when we receive the datapackage
	case is_integer(StreamId) of
		true->
			FieldValue1 = {"streamid":integer_to_binary(StreamId)};
		_ ->
			FieldValue1 = {"streamid":list_to_binary(StreamId)}
	end,
	case is_integer(Res) of
		true->
			FieldValue2 = {"value":integer_to_binary(Res)};
		_->
			case is_float(Res) of
				true->
					FieldValue2 = {"value":float_to_binary(Res)};
				_ ->
					FieldValue2 = {"value":list_to_binary(Res)}
			end
	end,
	{{Year, Month, Day}, {Hour, Minutes, Seconds}} = calendar:now_to_universal_time(os:timestamp()),
	
	StrYear = integer_to_list(Year),
	StrMonth = integer_to_list(Month),
	StrDay = integer_to_list(Day),
	StrHour = integer_to_list(Hour),
	StrMinutes = integer_to_list(Minutes),
	StrSeconds = integer_to_list(Seconds),
	
	FieldValue3 = {"timestamp":list_to_binary(StrYear++":"++StrMonth++":"++StrDay++" "++StrHour++":"++StrMinutes++":"++StrSeconds)},
	FieldValues = [FieldValue1, FieldValue2, FieldValue3],
	FinalJson = lib_json:add_values("{}", FieldValues),
	case erlastic_search:index_doc(?INDEX, "datapoint", FinalJson) of
		{error, Reason} -> erlang:display("Failed to insert the new datapoint into the elasticsearch for this reason: "++Reason),
						   {error, Reason};
		{ok,List} -> ok
	end.

%% @doc
%% Function: parseText/2
%% Purpose: used to parse the text data, unfinished and needs to be updated.
%% Returns: ok
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseText(record(), any()) -> ok.
parseText(Parser, Data) ->
	%% extract the wanted value from the text-data and store it in the DB
	%% return the status of the transaction, ok or {error, ErrMsg}
	ok.

%% @doc
%% Function: doParsing/3
%% Purpose: call parse function using the patterns contained in ParsersList to parse the data.
%% Returns: ok | {error, ErrMsg}
%% Side effects: Stores the newly parsed data points in the DB
%% @end
-spec doParsing(list(), any(), string()) -> ok | {error, string()}.
doParsing([], Data, ContentType) -> ok;
doParsing([Parser|ParsersList], Data, ContentType) ->
	case ContentType of
		"application/json"->
			TempRes = parseJson(Parser, Data),
			case TempRes of
				ok ->
					doParsing(ParsersList, Data, ContentType);
				{error, ErrMsg} ->
					%%parsing fails
					TempRes
			end;
		"plain/text"->
			TempRes = parseText(Parser, Data),
			case TempRes of
				ok ->
					doParsing(ParsersList, Data, ContentType);
				{error, ErrMsg} ->
					%%parsing fails
					TempRes
			end;
		_ ->
			%%error the content`s type doesn`t exist
			{error, "the content`s type doesn`t exist"}
	end.

%% @doc
%% Function: get_parsers_with_content_type/3
%% Purpose: find relative patterns for the specific content type.
%% Returns: The patterns suitable for the current content type
%% @end
-spec get_parsers_with_content_type(string(), list(), list()) -> list().
get_parsers_with_content_type(Content_type, [], L) -> L;
get_parsers_with_content_type(Content_type, [Parser|Parsers], L) ->
	case Parser#parser.input_type == Content_type of
		true -> get_parsers_with_content_type(Content_type, Parsers, [Parser|L]);
		_ -> get_parsers_with_content_type(Content_type, Parsers, L)
	end.