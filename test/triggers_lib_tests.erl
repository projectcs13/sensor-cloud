%% @author Tomas Sävström <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == triggers_lib_tests ==
%% This module contains several tests to test the functionallity
%% in the module triggers_lib.
%%
%% @end

-module(triggers_lib_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client.hrl").
-export([]).


%% @doc
%% Function: less_than_test/0
%% Purpose: Used test the less_than function
%% Returns: ok | {error, term()}
%%
%% @end
-spec less_than_test() -> ok | {error, term()}.

less_than_test() ->
	Data = [{"1","{\"value\":5}"},{"2","{\"value\":7}"},{"3","{\"value\":9}"},{"4","{\"value\":15}"}],
	Input = [{5,["Tomas"]},{7,["Erik"]},{9,["Johan"]},{15,["Jimmy"]}],
	Result = triggers_lib:less_than(Input,Data),
	ExpectedResult = [{5,"1",7,["Erik"]},{5,"1",9,["Johan"]},{5,"1",15,["Jimmy"]},{7,"2",9,["Johan"]},{7,"2",15,["Jimmy"]},{9,"3",15,["Jimmy"]}],
	?assertEqual(Result,ExpectedResult).

%% @doc
%% Function: greater_than_test/0
%% Purpose: Used test the less_than function
%% Returns: ok | {error, term()}
%%
%% @end
-spec greater_than_test() -> ok | {error, term()}.

greater_than_test() ->
	Data = [{"1","{\"value\":5}"},{"2","{\"value\":7}"},{"3","{\"value\":9}"},{"4","{\"value\":15}"}],
	Input = [{5,["Tomas"]},{7,["Erik"]},{9,["Johan"]},{15,["Jimmy"]}],
	Result = triggers_lib:greater_than(Input,Data),
	ExpectedResult = [{7,"2",5,["Tomas"]},{9,"3",5,["Tomas"]},{9,"3",7,["Erik"]},{15,"4",5,["Tomas"]},{15,"4",7,["Erik"]},{15,"4",9,["Johan"]}],
	?assertEqual(Result,ExpectedResult).

%% @doc
%% Function: span_test/0
%% Purpose: Used test the span function
%% Returns: ok | {error, term()}
%%
%% @end
-spec span_test() -> ok | {error, term()}.

span_test() ->
	Data = [{"1","{\"value\":5}"},{"2","{\"value\":7}"},{"3","{\"value\":9}"},{"4","{\"value\":15}"}],
	Input = [{[5,16],["Tomas"]},{[7,16],["Erik"]},{[9,16],["Johan"]},{[15,16],["Jimmy"]}],
	Result = triggers_lib:span(Input,Data),
	ExpectedResult = [{5,"1",[7,16],["Erik"]},{5,"1",[9,16],["Johan"]},{5,"1",[15,16],["Jimmy"]},{7,"2",[9,16],["Johan"]},{7,"2",[15,16],["Jimmy"]},{9,"3",[15,16],["Jimmy"]}],
	?assertEqual(Result,ExpectedResult).