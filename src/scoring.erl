%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == scoring ==
%% 
%%  
%%
%% @end

-module(scoring).

%% ====================================================================
%% API functions
%% ====================================================================
-export([calc/2]).

calc(Resource, resource) ->
	Manufacturer = lib_json:get_field(Resource, "manufacturer"),
	Tags = lib_json:get_field(Resource, "tags"),
	Polling_freq = lib_json:get_field(Resource, "polling_freq"),
	List = [Manufacturer, Tags, Polling_freq],
	Fun = fun(undefined, Acc) -> Acc;
			 (_,  Acc) -> Acc+1
		  end, 
	lists:foldr(Fun, 0, List).


%% ====================================================================
%% Internal functions
%% ====================================================================


