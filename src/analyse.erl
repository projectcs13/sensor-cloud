-module(analyse).
%-export([forecast/1, forecast/2, init/0, stop/0, this/0, get_analysis/2]).
-compile(export_all).

-include("webmachine.hrl").
-include("erlastic_search.hrl").
-define(INDEX, "sensorcloud").

-record(struct, {lst}).

%<<"[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]">>
%analyse:predict("[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]").

%% @doc
%% Function: allowed_methods/2
%% Purpose: Defines which HTTP methods are allowed
%% Returns: {List of allowed HTTP requests, string , string()}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
	erlang:display("Checking allowed methods!"),
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"streams", _StreamID}, {"_analyse"}] ->
		  	{['GET'], ReqData, State};
		[{"users", _UserID}, {"streams", _StreamID}, {"_analyse"}] ->
		  	{['GET'], ReqData, State};
		[{"resources", _ResourceID}, {"streams", _StreamID}, {"_analyse"}] ->
		  	{['GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", _StreamID}, {"_analyse"}] ->
		  	{['GET'], ReqData, State};
		[error] ->
		    {[], ReqData, State}
end.


%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::term(),State::term()) -> {list(), term(), term()}.
content_types_provided(ReqData, State) ->
	{[{"application/json", get_analysis}], ReqData, State}.

%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_accepted(ReqData, State) ->
	{[{"application/json", get_analysis}], ReqData, State}.


%% @doc
%% Function: delete_resource/2
%% Purpose: Deletes a resource and the streams associated with it
%% Returns:  ERROR = {{error,Errorcode} ReqData, State}
%%			 OK = {ok, ReqData, State}
%% @end
-spec delete_resource(ReqData::tuple(), State::string()) -> {string(), tuple(), string()}.
delete_resource(ReqData, State) ->
	erlang:display("delete resources!"),
	ok.

%% @doc
%% Function: process_post/2
%% Purpose: Handle POST request, only working for create and not search - AS OF SPRINT 3
%% Returns:  {JSON-object(string), ReqData, State}
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {atom(), tuple(), string()}.
process_post(ReqData, State) ->
	erlang:display("Process post!"),
    ok.


%% @doc
%% Function: get_analysis/2
%% Purpose: Used to handle GET requests by giving the document with the given
%% Id or listing the documents that can be found from the restrictions
%% given by the URI.
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec get_analysis(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
get_analysis(ReqData, State) ->
	erlang:display("Attempting to get analysis!"),
	case proplists:get_value('streamid', wrq:path_info(ReqData)) of
		undefined ->
			{{error,405}, wrq:set_resp_body("{\"error\":\"missing streamid\"}", ReqData), State};
		StreamId ->
			% Get specific stream
			% @TODO: Make sure data is sorted by timestamp!!!!! 
				
			case erlastic_search:search_limit(?INDEX, "datapoint","streamid:" ++ StreamId ++ "&sort=timestamp:asc", 20) of
			%case erlastic_search:search_json(#erls_params{}, ?INDEX, "datapoint", create_json(StreamId), []) of
				{error,Reason} ->
					io:format("Error on line 62 for " ++ StreamId),
					{{error,Reason}, ReqData, State};
				{ok,JsonStruct} -> {true,wrq:set_resp_body(forecast(lib_json:get_field(JsonStruct, "hits.hits")),ReqData),State}
			end
	end.


create_json(StreamId) ->
	"{ \"sort\" : [{ \"timestamp\" : {\"order\" : \"asc\"}}], \"query\" : { \"term\" : { \"streamid\" : \""++ StreamId ++ "\" }}}".



%% @doc
%% Function: init/0
%% Purpose: Initializes the analysis engine.
%% Returns: ok. 
%% @end
-spec start() -> ok.
start() ->
	Pid = eri:start(),
	eri:connect(),
	ok.

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
	%erlastic_search_app:start(), %% start this in the make file somehow
    {ok, undefined}.


%% @doc
%% Function: stop/0
%% Purpose: Tries to stop eri, but eri:stop seems to be broken.
%% Returns: CRASH.
%% @end
-spec stop() -> crash.
stop() ->
	eri:stop().


%% @doc
%% Function: this/0
%% Purpose: Does a test of forecast.
%% Returns: Json object with a list of the predictions.
%% @end
-spec this() -> JSON::string().
this() ->
	start(),
	forecast("[ { \"value\": 3347, \"date\": \"1995-06-09\" }, { \"value\": 1833, \"date\": \"1995-07-26\" }, { \"value\": 2470, \"date\": \"1996-11-19\" }, { \"value\": 2849, \"date\": \"1997-11-15\" }, { \"value\": 3295, \"date\": \"1998-10-01\" }, { \"value\": 2853, \"date\": \"1998-12-26\" }, { \"value\": 3924, \"date\": \"1999-11-23\" }, { \"value\": 1392, \"date\": \"2000-10-19\" }, { \"value\": 2127, \"date\": \"2001-03-09\" }, { \"value\": 2121, \"date\": \"2001-05-27\" }, { \"value\": 2817, \"date\": \"2002-05-03\" }, { \"value\": 1713, \"date\": \"2003-02-13\" }, { \"value\": 3699, \"date\": \"2003-05-25\" }, { \"value\": 2387, \"date\": \"2003-07-13\" }, { \"value\": 2409, \"date\": \"2004-01-11\" }, { \"value\": 3163, \"date\": \"2004-12-06\" }, { \"value\": 2168, \"date\": \"2005-10-05\" }, { \"value\": 1276, \"date\": \"2008-02-12\" }, { \"value\": 2597, \"date\": \"2009-12-29\" }, { \"value\": 2851, \"date\": \"2010-10-23\"}]").
	


%% @doc
%% Function: forecast/1
%% Purpose: Used to do a prediction with R given a json object. Uses 10 as a default number of predictions
%% Returns: List
%% @end
-spec forecast(JSON::string()) -> JSON::string().
forecast(Json) -> 
	erlang:display("In forecast!"),
	eri:eval("library(forecast)"),
	forecast(Json, 10).

%% @doc
%% Function: forecast/2
%% Purpose: Used to do a prediction with R given a json object, and the number of desired predicted datapoints
%% Returns: Json object with list of values
%% @end	
-spec forecast(JSON::string(), Nr::integer()) -> JSON::string().
forecast(Json, Nr) -> 
	erlang:display("Actually attempting to run forecast!"),
	{_Start, _End, Values} = get_time_series(Json),
	erlang:display(Values),
	Number = lists:flatten(io_lib:format("~p", [Nr])),
	eri:eval("A <- auto.arima(" ++ Values ++ ")"),
	eri:eval("pred <- forecast(A, "++ Number ++ ")"),
	{ok, _, Mean} = eri:eval("data.frame(c(pred$mean))[[1]]"),
	{ok, _, Lo80} = eri:eval("head(data.frame(c(pred$lower))[[1]], " ++ Number ++")"),
	{ok, _, Hi80} = eri:eval("head(data.frame(c(pred$upper))[[1]], " ++ Number ++")"),
	{ok, _, Lo95} = eri:eval("tail(data.frame(c(pred$lower))[[1]], " ++ Number ++")"),
	{ok, _, Hi95} = eri:eval("tail(data.frame(c(pred$upper))[[1]], " ++ Number ++")"),
	erlang:display("In forecast just before format!"),
	start_format_result({Mean, Lo80, Hi80, Lo95, Hi95}).


%% @doc
%% Function: start_format_result/1
%% Purpose: Format the results from a forecast.
%% Returns: Json object with list of values
%% @end	
-spec start_format_result({Mean::list(), Lo80::list(), Hi80::list(), Lo95::list(), Hi95::list()}) -> JSON::string().
start_format_result({Mean, Lo80, Hi80, Lo95, Hi95}) ->
	"{ \"predictions\": [" ++ format_result({Mean, Lo80, Hi80, Lo95, Hi95}).

%% @doc
%% Function: format_result/1
%% Purpose: Format the results from a forecast.
%% Returns: Everything in a Json object except the beginning.
%% @end	
-spec format_result({Mean::list(), Lo80::list(), Hi80::list(), Lo95::list(), Hi95::list()}) -> string().
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


%% @doc
%% Function: get_forecast_string/1
%% Purpose: Generates a string with a complete command to to forecast on Values in R
%% Returns: String with complete forecast command for R
%% @end	
-spec get_forecast_string(Values::string()) -> string().
get_forecast_string(Values) -> 
	"forecast(auto.arima("++Values++"))".


%% @doc
%% Function: get_time_series/1
%% Purpose: Gets information as strings from a Json object (first time, last time and a list with all values)
%% Returns: Data from JSON object as strings
%% @end
-spec get_time_series(JSON::string()) -> {Start::string(), End::string(), Values::string()}.
get_time_series(Json) ->
	%erlang:display(Json),
    %Data = mochijson2:decode(Json),
	{Values, Times} = parse_json_list(Json, [], []),
	{Start, End} = get_times(Times, {}),
	{Start, End, get_values_string(Values)}.


%% @doc
%% Function: parse_json_list/3
%% Purpose: Get a list of times and values from a Json object
%% Returns: Lists with data from list of Json objects lists
%% @end
-spec parse_json_list(Datapoint::list(), Values::list(), Times::list()) -> {Values::list(), Times::list()}.
parse_json_list([], Values, Times) -> {Values, Times};
parse_json_list([Head|Rest], Values, Times) ->
	Struct = proplists:get_value(<<"_source">>, Head#struct.lst),
	Val = proplists:get_value(<<"value">>, Struct#struct.lst),
	Time = proplists:get_value(<<"timestamp">>, Head#struct.lst),
	parse_json_list(Rest, [Val|Values], [Time|Times]).


%% @doc
%% Function: get_times/1
%% Purpose: Get the first and last time from a list (no longer interesting)
%% Returns: A tuple with the first and last times in the list.
%% @end	
-spec get_times(Values::string(), tuple()) -> {list(), list()}.
get_times([], {}) -> {"321", "123"};
get_times([End | List], {}) -> {"321", "123"};
get_times(List, {End}) -> {binary_to_list(lists:last(List)), binary_to_list(End)}.


%% @doc
%% Function: get_values_string/1
%% Purpose: Generates a string with a complete command to to forecast on Values in R
%% Returns: String with complete forecast command for R
%% @end	
-spec get_values_string(Values::string()) -> string().
get_values_string([]) -> "c()";
get_values_string([Head | Tail]) -> get_values_string_(Tail, lists:flatten(io_lib:format("~p)", [Head]))).


%% @doc
%% Function: get_values_string_/1
%% Purpose: Get a string with values formatted as an R command (to create an equivalent list in R)
%% Returns: A string with all the values in the argument
%% @end	
-spec get_values_string_(Values::list(), string()) -> string().
get_values_string_([], S) -> "c("++S;
get_values_string_([Head | Tail], S) -> get_values_string_(Tail, lists:flatten(io_lib:format("~p, ", [Head]))++S).




