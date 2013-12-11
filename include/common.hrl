%% @doc
%% Author: Gabriel Tholsgård, Li Hao
%% [www.csproj13.student.it.uu.se]
%% == common settings and names include file ==
%% Provides definitions and settings for common things
%%
%% @end

-ifndef(__COMMON_HRL__).
-define(__COMMON_HRL__, 1).


	%% IP address to Elastic Search server
	-ifndef(ES_IP_ADDR).
	-define(ES_IP_ADDR, "localhost").
	-endif.
	
	
	%% Port used by Elastic Search server
	-ifndef(ES_PORT).
	-define(ES_PORT, "9200").
	-endif.
	
	
	%% Index name of Elastic Search
	-ifndef(ES_INDEX).
	-define(ES_INDEX, "sensorcloud").
	-endif.
	
	
	%% HTTP URL to Elastic Search server (according to the set macros above)
	-ifndef(ES_ADDR).
	-define(ES_ADDR, "http://" ++ ?ES_IP_ADDR ++ ":" ++ ?ES_PORT ++ "/" ++ ?ES_INDEX).
	-endif.
	
	
	%% User Agent of httpc request
	-ifndef(UA).
	-define(UA, "sensor-cloud:").
	-endif.


	%% IP address to RabbitMQ server
	-define(RMQ_IPADDR, "localhost").


	%% Port used by RabbitMQ server
	-define(RMQ_PORT, "5672").


	%% HTTP URL to RabbitMQ server
	-define(RMQ_ADDR, ?RMQ_ADDR ++ ":" ++ ?RMQ_PORT).

-endif.
