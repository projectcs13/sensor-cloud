%% @author Tommy Mattsson <toma10293@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == json include file ==
%% Provides definitions for creating JSON string objects
%%
%% @end
-include("misc.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifndef(GENERIC_JSON).
-define(GENERIC_JSON, generic_json).
-endif.

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




-ifndef(JSON_STRUCT).
-define(JSON_STRUCT(ARG), ?TUPLE(string:join(lists:map(fun({Attr, Val}) -> ?JSON_ATTR(Attr, Val) end, ARG), ", "))).
-endif.



-ifndef(JSON_ATTR).
-define(JSON_ATTR(Attr, Val), 
	lists:concat([?QUOTE(?TO_STRING(Attr)), ?COLON,  
		      
		      %% indicates that Z is to be treated as a string value
		      fun({s, Z}) -> ?QUOTE(Z);
			 
			 %%indicates that Z will be treated as a list of values
			 ({l, Z}) -> Fun = fun(Y) -> ?QUOTE(?TO_STRING(Y)) end,
				     ?LIST(string:join(lists:map(Fun, Z), ", "))
		      end(Val)])).
-endif.
