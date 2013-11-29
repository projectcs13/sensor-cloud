-module(triggers_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([less_than/2,greater_than/2, span/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================
less_than(InputList, []) ->
	[];
less_than(InputList, [{StreamId,Data}|Rest]) ->
	less_than(InputList,StreamId,Data) ++ less_than(InputList,Rest).



less_than([], _StreamId,_Data) ->
	[];
less_than([{Threshold,Users}|Rest], StreamId,Data) ->
	
	Value = lib_json:get_field(Data, "value"),
	case Value < Threshold of
		true ->
			[{Value, StreamId, Threshold, Users}|less_than(Rest,StreamId,Data)];
		false ->
			less_than(Rest,StreamId,Data)
	end.

greater_than(InputList, []) ->
	[];
greater_than(InputList, [{StreamId,Data}|Rest]) ->
	greater_than(InputList,StreamId,Data) ++ greater_than(InputList,Rest).



greater_than([], _StreamId,_Data) ->
	[];
greater_than([{Threshold,Users}|Rest], StreamId,Data) ->
	
	Value = lib_json:get_field(Data, "value"),
	case Value > Threshold of
		true ->
			[{Value, StreamId, Threshold, Users}|greater_than(Rest,StreamId,Data)];
		false ->
			greater_than(Rest,StreamId,Data)
	end.

span(InputList, []) ->
	[];
span(InputList, [{StreamId,Data}|Rest]) ->
	span(InputList,StreamId,Data) ++ span(InputList,Rest).



span([], _StreamId,_Data) ->
	[];
span([{[Lower,Upper],Users}|Rest], StreamId,Data) ->
	
	Value = lib_json:get_field(Data, "value"),
	case (Value > Upper) or (Value < Lower) of
		true ->
			[{Value, StreamId, [Lower,Upper], Users}|span(Rest,StreamId,Data)];
		false ->
			span(Rest,StreamId,Data)
	end.

