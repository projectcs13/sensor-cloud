%% @author Tomas Sävström <tosa7943@student.uu.se>, Andreas Moregård Haubenwaller
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == triggers_lib ==
%% This module contains some functions that can be
%% called as trigger functions
%%
%% @end

-module(triggers_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([less_than/2,greater_than/2, span/2]).


%% @doc
%% Function: greater_than/2
%% Purpose: Call the trigger function greater_than and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{Threshold1,Users1},...] and a datalist like
%%          [{StreamId1,DataPoint1},....] where Datapoint is a json datapoint
%% Returns: [{Value1,StreamId1,Threshold1,Users1},...] for values where the
%%        	value is greater than threshold.
%% @end
-spec greater_than(InputList::list(),DataList::list()) -> AlertList::list().

greater_than(InputList, []) ->
	[];
greater_than(InputList, [{StreamId,Data}|Rest]) ->
	greater_than(InputList,StreamId,Data) ++ greater_than(InputList,Rest).


%% @doc
%% Function: less_than/2
%% Purpose: Call the trigger function less_than and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{Threshold1,Users1},...] and a datalist like
%%          [{StreamId1,DataPoint1},....] where Datapoint is a json datapoint
%% Returns: [{Value1,StreamId1,Threshold1,Users1},...] for values where the
%%        	value is less than threshold.
%% @end
-spec less_than(InputList::list(),DataList::list()) -> AlertList::list().

less_than(InputList, []) ->
	[];
less_than(InputList, [{StreamId,Data}|Rest]) ->
	less_than(InputList,StreamId,Data) ++ less_than(InputList,Rest).

%% @doc
%% Function: span/2
%% Purpose: Call the trigger function span and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{[Lower,Upper],Users1},...] and a datalist like
%%          [{StreamId1,DataPoint1},....] where Datapoint is a json datapoint
%% Returns: [{Value1,StreamId1,[Lower1,Upper1],Users1},...] for values where the
%%        	value is not in the interval created by Lower and Upper
%% @end
-spec span(InputList::list(),DataList::list()) -> AlertList::list().

span(InputList, []) ->
	[];
span(InputList, [{StreamId,Data}|Rest]) ->
	span(InputList,StreamId,Data) ++ span(InputList,Rest).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: greater_than/3
%% Purpose: Call the trigger function greater_than and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{Threshold1,Users1},...] and a streamid and a datapoint
%%          where Datapoint is a json datapoint with the value field
%% Returns: [{Value,StreamId1,Threshold1,Users1},...] the value where the
%%        	value is greater than threshold.
%% @end
-spec greater_than(InputList::list(),StreamId::string(),Datapoint::string()) -> AlertList::list().

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


%% @doc
%% Function: less_than/3
%% Purpose: Call the trigger function greater_than and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{Threshold1,Users1},...] and a streamid and a datapoint
%%          where Datapoint is a json datapoint with the value field
%% Returns: [{Value,StreamId1,Threshold1,Users1},...] for the value where the
%%        	value is less than threshold.
%% @end
-spec less_than(InputList::list(),StreamId::string(),Datapoint::string()) -> AlertList::list().

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



%% @doc
%% Function: span/3
%% Purpose: Call the trigger function span and create the list
%%          of alerts that are used to create messages. Expects a inputlist
%%          that is [{[Lower1,Upper1],Users1},...] and a streamid and a datapoint
%%          where Datapoint is a json datapoint with the value field
%% Returns: [{Value,StreamId1,[Lower1,Upper1],Users1},...] for the value where the
%%        	value is not in the interval created by Lower and Upper.
%% @end
-spec span(InputList::list(),StreamId::string(),Datapoint::string()) -> AlertList::list().

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

