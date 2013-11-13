%% Author: Tommy Mattsson <toma10293@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% == debug include file ==
%% Provides useful macros used for debugging. 
%%
%% @end
-define(INFO(X), ?GENERIC("INFO", X)).

-ifdef(debug).
-define(DEBUG(X), ?GENERIC("DEBUG", X)).
-else.
-define(DEBUG(X), true).
-endif.

-ifdef(debug).
-define(ERROR(X), ?GENERIC("ERROR", X)).
-else.
-define(ERROR(X), true).
-endif.

-define(GENERIC(TYPE, X), io:format("*** ~s: {MODULE: ~p}{LINE: ~p}{MSG: ~p} ***~n", [TYPE,?MODULE,?LINE,X])).


