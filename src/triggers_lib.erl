-module(triggers_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================


less_than(DataPoint,Threshold) ->
	Value = lib_json:get_field(DataPoint, "value"),
	Value < Threshold.

greater_than(DataPoint,Threshold) ->
	Value = lib_json:get_field(DataPoint, "value"),
	Value > Threshold.