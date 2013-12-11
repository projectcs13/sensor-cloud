%% @author Tomas Sävström <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == vs_func_lib ==
%% This module contains prefdefined functions used in virtual streams
%%
%% @end
-module(vs_func_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([min/2,max/2,mean/2,total/2]).
-compile({no_auto_import,[min/2]}).
-compile({no_auto_import,[max/2]}).

%% @doc
%% Function: min/1
%% Purpose: Used to calculate the min for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by min on the last list of data points
%% @end
-spec min(DataList::list(),StreamId::binary()) -> ResultList::list().
min(DataList,StreamId) ->
	min(DataList,StreamId,[]).

%% @doc
%% Function: max/1
%% Purpose: Used to calculate the max for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by max on the last list of data points
%% @end
-spec max(DataList::list(),StreamId::binary()) -> ResultList::list().
max(DataList,StreamId) ->
	max(DataList,StreamId,[]).

%% @doc
%% Function: mean/1
%% Purpose: Used to calculate the average for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by avg on the last list of data points
%% @end
-spec mean(DataList::list(),StreamId::binary()) -> ResultList::list().
mean(DataList,StreamId) ->
	avg(DataList,StreamId,[]).

%% @doc
%% Function: total/2
%% Purpose: Used to calculate the sum for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by sum on the last list of data points
%% @end
-spec total(DataList::list(),StreamId::binary()) -> ResultList::list().
total(DataList,StreamId) ->
	sum(DataList,StreamId,[]).

%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: min/3
%% Purpose: Used to calculate the min for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by min on the last list of data points
%% @end
-spec min(DataList::list(), StreamId::string(), Acc::list()) -> ResultList::list().

min([],_StreamId,Acc) ->
	Acc;
min([First|Rest],StreamId,Acc) ->
	DataPoint = min_internal(First,none,none,StreamId),
	min(Rest,StreamId,[DataPoint|Acc]).

%% @doc
%% Function: min_internal/4
%% Purpose: Used to calculate the min for 
%%          a given list of data points
%%          and create a data point with the given stream_id,
%%          expects a list like [datapoint1,datapoint2,...]
%%          where datapoints are json objects with value and timestamp
%% Returns: a datapoint that contains the min of the values in the list
%%          as the value, and the most recent timestamp as the timestamp
%% @end
-spec min_internal(DataList::list(),  Acc::list(), TimeStamp::atom() | string(), StreamId::string()) -> ResultList::list().

min_internal([],Acc,TimeStamp,StreamId) ->
	lib_json:set_attrs([{"value", Acc}, {"timestamp", TimeStamp}, {"stream_id", StreamId}]);
min_internal([First|Rest],Acc,TimeStamp,StreamId) ->
	case TimeStamp of
		none ->
			NewTimeStamp = lib_json:get_field(First, "timestamp");
		_ ->
			case lib_json:to_string(TimeStamp) < lib_json:to_string(lib_json:get_field(First, "timestamp")) of
				true ->
					NewTimeStamp = lib_json:get_field(First, "timestamp");
				false ->
					NewTimeStamp = TimeStamp
			end
	end,
	case Acc of 
		none ->
			min_internal(Rest,lib_json:get_field(First, "value"),NewTimeStamp,StreamId);
		_ ->
			case lib_json:get_field(First, "value") < Acc of
				true ->
					min_internal(Rest,lib_json:get_field(First, "value"),NewTimeStamp,StreamId);
				false ->
					min_internal(Rest,Acc,NewTimeStamp,StreamId)
			end
	end.


%% @doc
%% Function: min/3
%% Purpose: Used to calculate the max for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by max on the last list of data points
%% @end
-spec max(DataList::list(), StreamId::string(), Acc::list()) -> ResultList::list().

max([],_StreamId,Acc) ->
	Acc;
max([First|Rest],StreamId,Acc) ->
	DataPoint = max_internal(First,none,none,StreamId),
	max(Rest,StreamId,[DataPoint|Acc]).


%% @doc
%% Function: max_internal/4
%% Purpose: Used to calculate the min for 
%%          a given list of data points
%%          and create a data point with the given stream_id,
%%          expects a list like [datapoint1,datapoint2,...]
%%          where datapoints are json objects with value and timestamp
%% Returns: a datapoint that contains the max of the values in the list
%%          as the value, and the most recent timestamp as the timestamp
%% @end
-spec max_internal(DataList::list(),  Acc::list(), TimeStamp::atom() | string(), StreamId::string()) -> ResultList::list().


max_internal([],Acc,TimeStamp,StreamId) ->
	lib_json:set_attrs([{"value", Acc}, {"timestamp", TimeStamp}, {"stream_id", StreamId}]);
max_internal([First|Rest],Acc,TimeStamp,StreamId) ->
	case TimeStamp of
		none ->
			NewTimeStamp = lib_json:get_field(First, "timestamp");
		_ ->
			case lib_json:to_string(TimeStamp) < lib_json:to_string(lib_json:get_field(First, "timestamp")) of
				true ->
					NewTimeStamp = lib_json:get_field(First, "timestamp");
				false ->
					NewTimeStamp = TimeStamp
			end
	end,
	case Acc of 
		none ->
			max_internal(Rest,lib_json:get_field(First, "value"),NewTimeStamp,StreamId);
		_ ->
			case lib_json:get_field(First, "value") > Acc of
				true ->
					max_internal(Rest,lib_json:get_field(First, "value"),NewTimeStamp,StreamId);
				false ->
					max_internal(Rest,Acc,NewTimeStamp,StreamId)
			end
	end.


%% @doc
%% Function: avg/3
%% Purpose: Used to calculate the avg for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by avg on the last list of data points
%% @end
-spec avg(DataList::list(), StreamId::string(), Acc::list()) -> ResultList::list().

avg([],_StreamId,Acc) ->
	Acc;
avg([First|Rest],StreamId,Acc) ->
	DataPoint = avg_internal(First,{0,0},none,StreamId),
	avg(Rest,StreamId,[DataPoint|Acc]).


%% @doc
%% Function: avg_internal/4
%% Purpose: Used to calculate the avg for 
%%          a given list of data points
%%          and create a data point with the given stream_id,
%%          expects a list like [datapoint1,datapoint2,...]
%%          where datapoints are json objects with value and timestamp
%% Returns: a datapoint that contains the avg of the values in the list
%%          as the value, and the most recent timestamp as the timestamp
%% @end
-spec avg_internal(DataList::list(),  Acc::list(), TimeStamp::atom() | string(), StreamId::string()) -> ResultList::list().


avg_internal([],{Value,Number},TimeStamp,StreamId) ->
	lib_json:set_attrs([{"value", Value/Number}, {"timestamp", TimeStamp}, {"stream_id", StreamId}]);
avg_internal([First|Rest],{Value,Number},TimeStamp,StreamId) ->
	case TimeStamp of
		none ->
			NewTimeStamp = lib_json:get_field(First, "timestamp");
		_ ->
			case lib_json:to_string(TimeStamp) < lib_json:to_string(lib_json:get_field(First, "timestamp")) of
				true ->
					NewTimeStamp = lib_json:get_field(First, "timestamp");
				false ->
					NewTimeStamp = TimeStamp
			end
	end,
	avg_internal(Rest,{Value + lib_json:get_field(First, "value"),Number+1},NewTimeStamp,StreamId).


%% @doc
%% Function: sum/3
%% Purpose: Used to calculate the sum for 
%%          a given list of data points
%%          and create data points with the given stream_id, expects a list
%%          like [[datapoint1,datapoint2,...],[datapoint1,datapoint2,...],...]
%%          where datapoints are json objects with value and timestamp
%% Returns: [datapoint1,datapoint2,...] where the first datapoint is the
%%          one created by sum on the last list of data points
%% @end
-spec sum(DataList::list(), StreamId::string(), Acc::list()) -> ResultList::list().

sum([],_StreamId,Acc) ->
	Acc;
sum([First|Rest],StreamId,Acc) ->
	DataPoint = sum_internal(First,0,none,StreamId),
	sum(Rest,StreamId,[DataPoint|Acc]).

%% @doc
%% Function: sum_internal/4
%% Purpose: Used to calculate the sum for 
%%          a given list of data points
%%          and create a data point with the given stream_id,
%%          expects a list like [datapoint1,datapoint2,...]
%%          where datapoints are json objects with value and timestamp
%% Returns: a datapoint that contains the sum of the values in the list
%%          as the value, and the most recent timestamp as the timestamp
%% @end
-spec sum_internal(DataList::list(),  Acc::list(), TimeStamp::atom() | string(), StreamId::string()) -> ResultList::list().


sum_internal([],Acc,TimeStamp,StreamId) ->
	lib_json:set_attrs([{"value", Acc}, {"timestamp", TimeStamp}, {"stream_id", StreamId}]);
sum_internal([First|Rest],Acc,TimeStamp,StreamId) ->
	case TimeStamp of
		none ->
			NewTimeStamp = lib_json:get_field(First, "timestamp");
		_ ->
			case lib_json:to_string(TimeStamp) < lib_json:to_string(lib_json:get_field(First, "timestamp")) of
				true ->
					NewTimeStamp = lib_json:get_field(First, "timestamp");
				false ->
					NewTimeStamp = TimeStamp
			end
	end,
	sum_internal(Rest,Acc + lib_json:get_field(First, "value"),NewTimeStamp,StreamId).
