%% @doc
%% Author: Gabriel Tholsgård, Li Hao
%% [www.csproj13.student.it.uu.se]
%% == common settings and names include file ==
%% Provides definitions and settings for common things
%%
%% @end



%% Index name of Elastic Search
-ifndef(ES_INDEX).
-define(ES_INDEX, "sensorcloud").
-endif.




%% User Agent of httpc request
-ifndef(UA).
-define(UA, "sensor-cloud:").
-endif.

