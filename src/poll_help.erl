%% @author Gabriel Tholsg�rd, Li Hao
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == poll_help ==
%% This module contains helper functions needed for the polling system 
%%
%% @end

-module(poll_help).

-include("common.hrl").
-include_lib("erlastic_search.hrl").
-include("erlson.hrl").
-include("json.hrl").
-include("poller.hrl").
-include("pubsub.hrl").
-include("field_restrictions.hrl").

-export([get_streams_using_polling/0,
		 json_to_record_streams/1,
		 json_to_record_stream/1,
		 create_poller_history/1,
		 add_success/1,
		 add_failed/2]).




%% ====================================================================
%% API functions
%% ==================================================================== 

%% @doc
%% Function: get_streams_using_polling/0
%% Purpose: Retrieves all streams from Elastic Search that are using polling.
%% Returns: [] | [Stream, ... ] | {error, Reason}.
%% @end
-spec get_streams_using_polling() -> [] | [json_string()] | {error, term()}.
get_streams_using_polling() ->
	JsonQuery = "{\"size\":10000, \"query\": {\"term\":{\"polling\":true}}, "++
								 "\"filter\": {\"exists\": {\"field\":\"uri\"}}}",
	
	case erlastic_search:search_json(#erls_params{},
									 ?ES_INDEX,
									 "stream",
									 JsonQuery) of
		{error, Reason} -> {error, Reason};
		{ok, Result} ->
			lib_json:get_field(Result, "hits.hits")
	end.
	



%% @doc
%% Function: json_to_record_streams/1
%% Purpose: Converts a list of stream Jsons to a list of pollerInfo records
%% Returns: [] | [Stream, ...]
%% @end
-spec json_to_record_streams([json_string()]) -> [] | [record()].
json_to_record_streams([]) -> [];
json_to_record_streams([H|T]) ->
	[json_to_record_stream(H) | json_to_record_streams(T)].




%% @doc
%% Function: json_to_record_stream/1
%% Purpose: Converts a stream Json to a pollerInfo record
%% Returns: #pollerInfo{}
%% @end
-spec json_to_record_stream(Stream::json_string()) -> record().
json_to_record_stream(Stream) ->
	Name = case lib_json:get_field(Stream, "_source.name") of
			   undefined -> undefined;
			   N -> binary_to_list(N)
		   end,
	Uri = case lib_json:get_field(Stream, "_source.uri") of
			  undefined -> undefined;
			  U -> binary_to_list(U)
		  end,
	DataType = case lib_json:get_field(Stream, "_source.data_type") of
				   undefined -> undefined;
				   D -> binary_to_list(D)
			   end,
	ParserString = case lib_json:get_field(Stream, "_source.parser") of
					   undefined -> undefined;
					   P -> binary_to_list(P)
				   end,
	#pollerInfo{stream_id = binary_to_list(lib_json:get_field(Stream, "_id")),
				name = Name,
				uri = Uri,
				frequency = lib_json:get_field(Stream, "_source.polling_freq"),
				data_type = DataType,
				parser = ParserString
			   }.

%% @doc
%% Function: create_poller_history/1
%% Purpose: creates an empty polling history for
%%          the given stream
%% Returns: ok or {Code,Body} if there was an error in ES
%% @end
-spec create_poller_history(StreamId::string()) -> ok | {integer(),string()}.

create_poller_history(StreamId) ->
	NewHistory = lib_json:set_attrs([{"history","[]"}]),
	case erlastic_search:index_doc_with_id(?INDEX, "pollinghistory", StreamId, NewHistory) of	
		{error,{Code,Body}} ->
			{Code,Body};
		{ok,_List} -> 
			ok
	end.
create_poller_history(StreamId, Message) ->
	NewHistory = lib_json:set_attrs([{"history","["++Message++"]"}]),
	case erlastic_search:index_doc_with_id(?INDEX, "pollinghistory", StreamId, NewHistory) of	
		{error,{Code,Body}} ->
			{Code,Body};
		{ok,_List} -> 
			ok
	end.

%% @doc
%% Function: add_failed/1
%% Purpose: Updates the polling history with an error message
%% Returns: ok or {error,{Code,Body}} if there was an error in ES
%% @end
-spec add_failed(StreamId::string(),Error::atom()) -> ok | {atom(),{integer(),string()}}.

add_failed(StreamId,connection_error) ->
	Time = ?TIME_NOW(erlang:localtime()),
	UserId = case erlastic_search:get_doc(?INDEX, "stream", StreamId) of
				 {error, Reason} -> 
					 error;
				 {ok,List} -> 
					 lib_json:get_field(List, "_source.user_id")
			 end,
	Message = lib_json:set_attrs([{"polling","{}"},{"polling.stream",list_to_binary(StreamId)},{"polling.action",list_to_binary("error")},{"polling.message",list_to_binary("Connection Error")},{"polling.timestamp",list_to_binary(Time)}]),
    UpdateJson = "{\"script\":\"ctx._source.notifications += msg\",\"params\":{\"msg\":"++ Message ++"}}",
    case api_help:update_doc(?INDEX, "user", UserId, UpdateJson, []) of
        {error, {Code, Body}} ->
            {error, {Code, Body}};
        {ok, Response} ->
            ok
    end,
	UpdateJson2 = "{\"script\":\"if (ctx._source.history.size() == 100){ctx._source.history.remove((Object) ctx._source.history[0]);ctx._source.history += msg}{ctx._source.history += msg} \",\"params\":{\"msg\":"++ Message ++"}}",
	case api_help:update_doc(?INDEX, "pollinghistory", StreamId, UpdateJson2, []) of
	{error, {404, Body2}} -> create_poller_history(StreamId, Message);
        {error, {Code2, Body2}} ->
			erlang:display("Error when updateing pollinghistory for " ++ StreamId),
            {error, {Code2, Body2}};
        {ok, Response2} ->
            ok
    end;

add_failed(StreamId,elasticsearch_error) ->
	Time = ?TIME_NOW(erlang:localtime()),
	UserId = case erlastic_search:get_doc(?INDEX, "stream", StreamId) of
				 {error, Reason} -> 
					 error;
				 {ok,List} -> 
					 lib_json:get_field(List, "_source.user_id")
			 end,
	Message = lib_json:set_attrs([{"polling","{}"},{"polling.stream",list_to_binary(StreamId)},{"polling.message",list_to_binary("Could not save datapoint")},{"polling.action",list_to_binary("error")},{"polling.timestamp",list_to_binary(Time)}]),
	UpdateJson = "{\"script\":\"ctx._source.notifications += msg\",\"params\":{\"msg\":"++ Message ++"}}",
    case api_help:update_doc(?INDEX, "user", UserId, UpdateJson, []) of
        {error, {Code, Body}} ->
            {error, {Code, Body}};
        {ok, Response} ->
            ok
    end,
	UpdateJson2 = "{\"script\":\"if (ctx._source.history.size() == 100){ctx._source.history.remove((Object) ctx._source.history[0]);ctx._source.history += msg}{ctx._source.history += msg} \",\"params\":{\"msg\":"++ Message ++"}}",
	case api_help:update_doc(?INDEX, "pollinghistory", StreamId, UpdateJson2, []) of
	{error, {404, Body2}} -> create_poller_history(StreamId, Message);
        {error, {Code2, Body2}} ->
			erlang:display("Error when updateing pollinghistory for " ++ StreamId),
            {error, {Code2, Body2}};
        {ok, Response2} ->
            ok
    end.


%% @doc
%% Function: add_success/1
%% Purpose: Updates the polling history with a created datapoint message
%% Returns: ok or {error,{Code,Body}} if there was an error in ES
%% @end
-spec add_success(StreamId::string()) -> ok | {atom(),{integer(),string()}}.

add_success(StreamId) ->
	Time = ?TIME_NOW(erlang:localtime()),
	Message = lib_json:set_attrs([{"polling","{}"},{"polling.stream",list_to_binary(StreamId)},{"polling.message",list_to_binary("Created new datapoint")},{"polling.action",list_to_binary("create")},{"polling.timestamp",list_to_binary(Time)}]),
	UpdateJson = "{\"script\":\"if (ctx._source.history.size() == 100){ctx._source.history.remove((Object) ctx._source.history[0]);ctx._source.history += msg}{ctx._source.history += msg} \",\"params\":{\"msg\":"++ Message ++"}}",
	case api_help:update_doc(?INDEX, "pollinghistory", StreamId, UpdateJson, []) of
	{error, {404, Body2}} -> create_poller_history(StreamId, Message);
        {error, {Code, Body}} ->
			erlang:display("Error when updateing pollinghistory for " ++ StreamId),
            {error, {Code, Body}};
        {ok, Response} ->
            ok
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
	

















