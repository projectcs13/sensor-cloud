-module(analyse).
-export([get_time_series/1, get_arima_string/1, predict/1, predict/2, init/0, stop/0, this/0]).

-include("webmachine.hrl").

-record(struct, {lst}).

%<<"[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]">>
%analyse:predict("[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]"). 

init() ->
	Pid = eri:start(),
	eri:connect(),
	eri:eval("library(forecast)").

stop() ->
	eri:stop().

this() ->
	init(),
	predict("[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]").
	


%% @doc
%% Function: predict/1
%% Purpose: Used to do a prediction with R given a json object
%% Returns: List
%% @end

predict(Json) -> 
	predict(Json, 10).

%% @doc
%% Function: predict/2
%% Purpose: Used to do a prediction with R given a json object, and
%% Returns: List
%% @end	
predict(Json, Nr) -> 
	{_Start, _End, Values} = get_time_series(Json),
	Number = lists:flatten(io_lib:format("~p", [Nr])),
	eri:eval("A <- auto.arima(" ++ Values ++ ")"),

	eri:eval("pred <- forecast(A, "++ Number ++ ")"),

	{ok, _, Mean} = eri:eval("data.frame(c(pred$mean))[[1]]"),
	{ok, _, Lo80} = eri:eval("head(data.frame(c(pred$lower))[[1]], " ++ Number ++")"),
	{ok, _, Hi80} = eri:eval("head(data.frame(c(pred$upper))[[1]], " ++ Number ++")"),
	{ok, _, Lo95} = eri:eval("tail(data.frame(c(pred$lower))[[1]], " ++ Number ++")"),
	{ok, _, Hi95} = eri:eval("tail(data.frame(c(pred$upper))[[1]], " ++ Number ++")"),
	start_format_result({Mean, Lo80, Hi80, Lo95, Hi95}).

start_format_result({Mean, Lo80, Hi80, Lo95, Hi95}) ->
	"{ \"predictions\": [" ++ format_result({Mean, Lo80, Hi80, Lo95, Hi95}).


format_result({[HeadMean|[]], [HeadLo80|[]], [HeadHi80|[]],[HeadLo95|[]], [HeadHi95|[]]}) ->
	"{ \"value\":" ++ lists:flatten(io_lib:format("~p", [HeadMean])) ++ 
	",\"lo80\":" ++ lists:flatten(io_lib:format("~p", [HeadLo80])) ++
	",\"hi80\":" ++ lists:flatten(io_lib:format("~p", [HeadHi80])) ++ 
	",\"lo95\":" ++ lists:flatten(io_lib:format("~p", [HeadLo95])) ++ 
	",\"hi95\":" ++ lists:flatten(io_lib:format("~p", [HeadHi95])) ++ "}]}";
format_result({[HeadMean|Mean], [HeadLo80|Lo80], [HeadHi80|Hi80],[HeadLo95|Lo95], [HeadHi95|Hi95]}) ->
	"{ \"value\":" ++ lists:flatten(io_lib:format("~p", [HeadMean])) ++ 
	",\"lo80\":" ++ lists:flatten(io_lib:format("~p", [HeadLo80])) ++ 
	",\"hi80\":" ++ lists:flatten(io_lib:format("~p", [HeadHi80])) ++ 
	",\"lo95\":" ++ lists:flatten(io_lib:format("~p", [HeadLo95])) ++ 
	",\"hi95\":" ++ lists:flatten(io_lib:format("~p", [HeadHi95])) ++ "},"
	 ++  format_result({Mean, Lo80, Hi80, Lo95, Hi95}).



get_arima_string(Values) -> 
	"forecast(auto.arima("++Values++"))".


%% @doc
%% Function: json_get_time_series/1
%% Purpose: Used to transform the given json to data
%% Returns: Data from JSON object
%% @end

get_time_series(Json) ->
    Data = mochijson2:decode(Json),
	{Values, Times} = parse_json_list(Data, [], []),
	{Start, End} = get_times(Times, {}),
	{Start, End, get_values_string(Values)}.


parse_json_list([], Values, Times) -> {Values, Times};
parse_json_list([Head|Rest], Values, Times) -> 
	Val = proplists:get_value(<<"value">>, Head#struct.lst),
	Time = proplists:get_value(<<"date">>, Head#struct.lst),
	parse_json_list(Rest, [Val|Values], [Time|Times]).


get_times([End | List], {}) -> get_times(List, {End});
get_times(List, {End}) -> {binary_to_list(lists:last(List)), binary_to_list(End)}.


get_values_string([Head | Tail]) -> get_values_string_(Tail, lists:flatten(io_lib:format("~p)", [Head]))).

get_values_string_([], S) -> "c("++S;
get_values_string_([Head | Tail], S) -> get_values_string_(Tail, lists:flatten(io_lib:format("~p, ", [Head]))++S).


