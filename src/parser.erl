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
-include("common.hrl").
-include("pubsub.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([parseJson/4, parseText/4]).

%% @doc
%% Function: parseJson/2
%% Purpose: used to parse the json data.
%% Parameters: StreamId --the id of stream
%% 			   Parser   --the parser record, defined in parser.hrl file
%%             Data     --the data which is going to be parsed, should be a json string
%%             Time 	--the time of the date area in http response header
%% Returns: JsonData | {error, ErrMsg}
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseJson(StreamId :: string(), Parser :: tuple(), Data :: string(), Time :: list()) -> string() | {error, string()}.
parseJson(StreamId, Parser, Data, Time) ->
	ItemList = filename:split(Parser),
	Query = processParser(ItemList, []),
	Res = lib_json:get_field(Data, Query),
	
	%%insert the data as a new datapoint into the database
	FieldValue1 = case is_integer(StreamId) of
					true->	
						{"stream_id",integer_to_binary(StreamId)};
					_ ->
						{"stream_id",list_to_binary(StreamId)}
				  end,
	FieldValue2 = {"value", api_help:any_to_float(Res)},
	TimeStamp = case Time of
					[] ->
						list_to_binary(?TIME_NOW(erlang:localtime()));
					_->
						list_to_binary(Time)
				end,
	
	FieldValue3 = {"timestamp",TimeStamp},
	FieldValues = [FieldValue1, FieldValue2, FieldValue3],
	FinalJson = lib_json:add_values("{}", FieldValues),
	
	case erlastic_search:index_doc(?ES_INDEX, "datapoint", FinalJson) of
		{error, Reason} -> erlang:display("Failed to insert the new datapoint into the elasticsearch for this reason: "++Reason),
						   poll_help:add_failed(StreamId,elasticsearch_error),
						   {error, Reason};
		{ok,_List} -> 
			poll_help:add_success(StreamId),
			case datapoints:update_fields_in_stream({"stream", StreamId}, binary_to_list(TimeStamp)) of
				{error, _Code, ErrorString}->
					erlang:display("failed to update the stream`s information: "++ErrorString);
				_->
					ok
			end,
			FinalJson
	end.

%% @doc
%% Function: parseText/2
%% Purpose: used to parse the text data, unfinished and needs to be updated.
%% Returns: JsonData | {error, ErrMsg}
%% Side effects: Stores the newly parsed data point in the DB
%% @end
-spec parseText(list(), list(), any(), list()) -> string() | tuple().
parseText(_StreamId, _Parser, _Data, _Time) ->
	%% extract the wanted value from the text-data and store it in the DB
	%% return the status of the transaction, ok or {error, ErrMsg}
	{error, "parseText function has not been implemented"}.




%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc
%% Function: processParser/2
%% Purpose: transforms the format of the parser
%% Example: ["streams","temperature","value"] => "streams.temperature.value" 
%% Returns: string()
%% @end
-spec processParser(list(string()), list(string())) -> string().
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


