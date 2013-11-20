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
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([parseJson/2, parseText/2]).

%% @doc
%% Function: parseJson/2
%% Purpose: used to parse the json data, unfinished and needs to be updated.
%% Returns: ok | {error, ErrMsg}
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseJson(tuple(), any()) -> ok.
parseJson(Parser, Data) ->

	%%extract the data from the coming data
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
			FieldValue1 = {"stream_id",integer_to_binary(StreamId)};
		_ ->
			FieldValue1 = {"stream_id",list_to_binary(StreamId)}
	end,
	case is_integer(Res) of
		true->
			FieldValue2 = {"value",integer_to_binary(Res)};
		_->
			case is_float(Res) of
				true->
					FieldValue2 = {"value",float_to_binary(Res)};
				_ ->
					FieldValue2 = {"value",list_to_binary(Res)}
			end
	end,
	{{Year, Month, Day}, {Hour, Minutes, Seconds}} = calendar:now_to_universal_time(os:timestamp()),
	
	StrYear = integer_to_list(Year),
	StrMonth = integer_to_list(Month),
	StrDay = integer_to_list(Day),
	StrHour = integer_to_list(Hour),
	StrMinutes = integer_to_list(Minutes),
	StrSeconds = integer_to_list(Seconds),
	
	FieldValue3 = {"timestamp",list_to_binary(StrYear++":"++StrMonth++":"++StrDay++" "++StrHour++":"++StrMinutes++":"++StrSeconds)},
	FieldValues = [FieldValue1, FieldValue2, FieldValue3],
	FinalJson = lib_json:add_values("{}", FieldValues),
	
	case erlastic_search:index_doc(?ES_INDEX, "datapoint", FinalJson) of
		{error, Reason} -> erlang:display("Failed to insert the new datapoint into the elasticsearch for this reason: "++Reason),
						   {error, Reason};
		{ok,List} -> 
			%% only for testing
			erlang:display("the final data which is inserted into the database: "++FinalJson),
			
			ok
	end.

%% @doc
%% Function: parseText/2
%% Purpose: used to parse the text data, unfinished and needs to be updated.
%% Returns: ok
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseText(tuple(), any()) -> ok.
parseText(Parser, Data) ->
	%% extract the wanted value from the text-data and store it in the DB
	%% return the status of the transaction, ok or {error, ErrMsg}
	ok.




%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: processParser/2
%% Purpose: transformat the format of the parser
%% Example: ["streams","temperature","value"] => "streams.temperature.value" 
%% Returns: string()
%% @end
-spec processParser(list(string()), list(string())) -> list(string()).
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