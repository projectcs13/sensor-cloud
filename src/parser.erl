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

%% ====================================================================
%% API functions
%% ====================================================================
-export([applyParser/3]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: parseJson/2
%% Purpose: used to parse the json data, unfinished and needs to be updated.
%% Returns: ok
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseJson(record(), any()) -> ok.
parseJson(Parser, Data) ->
	%% extract the wanted value from the json-data and store it in the DB
	%% return the status of the transaction, ok or {error, ErrMsg}
	ok.

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