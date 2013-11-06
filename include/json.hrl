%% Author: Tommy Mattsson <toma10293@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% == json include file ==
%% Provides definitions for creating JSON string objects
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @type attr() = atom() | string()
-type attr() :: atom() | string().
%% @type field() = json_string() | mochijson()
-type field() :: atom() | string() | [atom()].
%% @type json() = json_string() | mochijson()
-type json() :: json_string() | mochijson().
%% @type json_string() = string()
-type json_string() :: string().
%% @type json_input_value() = atom() | binary() | integer() | string() | json() | [json()]
-type json_input_value() :: atom() | binary() | integer() | json() | [json_input_value()].
%% @type json_output_value() = integer() | string() | json_string() | [json_output_value()]
-type json_output_value() :: boolean() | binary() | integer() | json_string() | [json_output_value()].
%% @type mochijson() = tuple() 
-type mochijson() :: tuple(). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Convenience Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifndef(QUOTE).
-define(QUOTE(ARG), "\"" ++ ARG ++ "\"").
-endif.

-ifndef(LIST).
-define(LIST(ARG), "[" ++ ARG ++ "]").
-endif.

-ifndef(TUPLE).
-define(TUPLE(ARG), "{" ++ ARG ++ "}").
-endif.

-ifndef(COLON).
-define(COLON, ":").
-endif.
