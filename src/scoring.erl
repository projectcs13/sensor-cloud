%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @headerfile "json.hrl"
%% @copyright [Copyright information]
%% @doc == Library for calculation scores (for suggestions) ==
%% @end

-module(scoring).
-include("json.hrl").

-export([calc/1, 
	 calc/2 
	]).

%% ====================================================================
%% API functions
%% ====================================================================

% @doc
% Calculates the number of non "undefined" in the input list. It is used as a scoring
% mechanism for suggestions
% @end
-spec calc(List::list()) -> integer().
calc(List) when is_list(List)->
	Fun = fun(undefined, Acc) -> Acc;
		("", Acc) -> Acc;
		(<<>>, Acc) -> Acc;
		(_,  Acc) -> Acc+1
	end, 
	lists:foldr(Fun, 0, List).


% @doc
% Calculates the score for a given resource. It is used as a scoring
% mechanism for suggestions
% @end
-spec calc(Resource::json(), atom()) -> integer().
calc(Resource, resource) ->
	Manufacturer = lib_json:get_field(Resource, "manufacturer"),
	Tags = lib_json:get_field(Resource, "tags"),
	Polling_freq = lib_json:get_field(Resource, "polling_freq"),
	List = [Manufacturer, Tags, Polling_freq],
	calc(List);
calc(Stream, stream) ->
	Name = lib_json:get_field(Stream, "name"),
	Description = lib_json:get_field(Stream, "description"),
	Min_val  = lib_json:get_field(Stream, "min_val"),
	Max_val  = lib_json:get_field(Stream, "max_val"),
	Tags  = lib_json:get_field(Stream, "tags"),
	Type  = lib_json:get_field(Stream, "type"),
	Accuracy  = lib_json:get_field(Stream, "accuracy"),
	calc([Name, Description, Min_val, Max_val, Tags, Type, Accuracy]).




%% ====================================================================
%% Internal functions
%% ====================================================================


