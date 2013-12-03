%% @author Gabriel Tholsgård, Li Hao
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
-include("parser.hrl").
-include("pubsub.hrl").

-export([get_streams_using_polling/0,
		 json_to_record_streams/1,
		 json_to_record_stream/1,
		 get_parser_by_id/1]).




%% ====================================================================
%% API functions
%% ====================================================================



%% @doc
%% Function: get_parser_by_id/1
%% Purpose: get parser according to specific stream id 
%% Returns: {error, ErrMsg} | #parser
%% @end
-spec get_parser_by_id(StreamId :: string()) -> {error, string()} | record().
get_parser_by_id(StreamId)->	
	case erlastic_search:search_limit(?ES_INDEX, "parser", "stream_id:\"" ++ StreamId++"\"", 100) of
		{ok, Result} ->
			EncodedResult = lib_json:encode(Result),
			case re:run(EncodedResult, "\"max_score\":null", [{capture, first, list}]) of
				{match, _} -> {error, "the parser not found"};
				nomatch -> FinalJsonList = lib_json:get_field(lib_json:get_list_and_add_id(Result), "hits"),
						   case length(FinalJsonList) of
							   1 ->
								   Item = lists:nth(1, FinalJsonList),
								   #parser{stream_id = binary_to_list(lib_json:get_field(Item, "stream_id")),
				  						input_parser = binary_to_list(lib_json:get_field(Item, "input_parser")),
				  						input_type = binary_to_list(lib_json:get_field(Item, "input_type"))
										 };
							   _ ->
								   erlang:display("multiple parsers exist for this stream id"),
								   {error, "multiple parsers exit for this stream id"}
						   end
			end;
		_ ->
			erlang:display("an error happens: the parser not found"),
			{error, "parsers not found!!"}
	end. 

%% @doc
%% Function: get_streams_using_polling/0
%% Purpose: Retrieves all streams from Elastic Search that are using polling.
%% Returns: [] | [Stream, ... ] | {error, Reason}.
%% @end
-spec get_streams_using_polling() -> [] | [json_string()] | {error, term()}.
get_streams_using_polling() ->
	JsonQuery = "{\"query\" : {\"filtered\" : " ++
					"{ \"filter\" : {\"exists\" : {\"field\" : \"uri\"}}}}}",
	
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
	#pollerInfo{stream_id = binary_to_list(lib_json:get_field(Stream, "_id")),
				name = Name,
				uri = Uri,
				frequency = lib_json:get_field(Stream, "_source.polling_freq")}.


%% ====================================================================
%% Internal functions
%% ====================================================================
	

















