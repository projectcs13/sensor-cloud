%% @author Jacob Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%% This module will contain all functions needed to handle 
%% http requests done to the webmachine regarding virtual streams 
%%
%% @end

-module(virtual_streams).
-export([init/1, allowed_methods/2, content_types_provided/2,
		 process_post/2]).

-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").

-define(INDEX, "sensorcloud").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
    {ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec allowed_methods(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of	
		[{"vstreams"}] ->
			{['POST'], ReqData, State}; 
		[{"vstreams", _Id}] -> %to be removed?
			{['POST'], ReqData, State};
		[{"vstreams", _Id}, {"data"}] -> %do we need this?
			{['POST', 'DELETE'], ReqData, State};
		[error] ->
			{[], ReqData, State}
	end.

%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::tuple(), State::string()) -> {list(), tuple(), string()}.
content_types_provided(ReqData, State) ->
		{[{"application/json", get_datapoint}], ReqData, State}.

%% @doc
%% Function: process_post/2
%% Purpose: decodes a JSON object and either adds the new datapoint in the DB or
%% performs search in the Datapoint database.
%% It is run automatically for POST requests
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
%%
%% Side effects: Inserts a new Datapoint in the database (when for insertion)
%% @end
-spec process_post(ReqData::tuple(), State::string()) -> {true, tuple(), string()}.
process_post(ReqData, State) ->
	{VirtualStreamJson,_,_} = api_help:json_handler(ReqData, State),
	{{Year,Month,Day},_} = calendar:local_time(),
	Date = generate_date([Year,Month,Day]),
	DateAdded = api_help:add_field(VirtualStreamJson,"creation_date",Date),
	case erlastic_search:index_doc(?INDEX, "vstream", DateAdded) of	
		{error, Reason} -> 
		VirtualStreamId = "x86vOMKiSIyLd5pQXDYi8w",	% to be fixed to do a good check
			{{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
		{ok,List} -> 
			VirtualStreamId = lib_json:get_field(List, "_id"),
			{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
	end,
	%these should be in the ok case of index_doc?
	StreamsInvolved = lib_json:get_field(VirtualStreamJson, "streams"),
	TimestampFrom = lib_json:get_field(VirtualStreamJson, "timestampfrom"),
	Function = lib_json:get_field(VirtualStreamJson, "function"),
	reduce(VirtualStreamId, StreamsInvolved, TimestampFrom, Function, ReqData, State)
	.


%% @doc
%% Function: reduce/6
%% Purpose: Gets information about which streams will be used and 
%% optionally a timestamp range, it executes a query and posts the datapoints returned
%% to the current virtual stream
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}
-spec reduce(VirtualStreamId::string(), Streams::string(), TimestampFrom::string(), Function::string(), ReqData::tuple(), State::string()) -> %%should change to date type instead, also add ReqData, State
		  {true, tuple(), string()}.
reduce(VirtualStreamId, Streams, TimestampFrom, Function, ReqData, State) ->
	% (aggregated *) average, min, max, std deviation, count, sum of squares, variance
	Query = create_query(Function, Streams, TimestampFrom),
   
	% should also update the connection of vstream <--> streams

	% in the statistics query the I can get the "statistical" field to push
		case erlastic_search:search_json(#erls_params{},?INDEX, "datapoint", lib_json:to_string(Query)) of
				{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
				{ok,JsonStruct} ->
					%%		check what happens if list is empty, improve with bulk posting
					%%   	{true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State},
			erlang:display(lib_json:to_string(JsonStruct)),
							DatapointsList = lib_json:get_field(JsonStruct, "hits.hits"),
						
							NewDatapoints = lists:map(fun(Json) -> 
														RmId = lib_json:get_field(Json, "_source"),
														FinalDatapoint = lib_json:replace_field(RmId, "streamid", VirtualStreamId),
													  	erlastic_search:index_doc(?INDEX, "datapoint", FinalDatapoint)
													 end, DatapointsList),
							
							%%will post all datapoints one by one, improve with bulk posting should be like the following
%% 							case erlastic_search:bulk_index_docs(?INDEX, "datapoint", NewDatapoints) of
%% 									{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
%% 									{ok,List} -> {true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
%% 							end
							{true, wrq:set_resp_body("\"status\":\"ok\"", ReqData), State} %% need to fix message returned
		end.

 
%% @doc
%% Function: create_query/3
%% Purpose: Creates the query for the function specified
%% Returns: string()
%% @end
-spec create_query(Function::string(), Streams::string(), TimestampFrom::string()) -> {string()}.
create_query(Function, Streams, TimestampFrom) ->
	%size to be passed as a variable??
	Query = lib_json:set_attrs(
		  [{size, 100},
		   {"query", "{}"},
		   {"query.filtered", "{}"},
		   {"query.filtered.query", "{}"},
		   {"query.filtered.query.terms", "{}"},
		   {"query.filtered.query.terms.streamid", Streams},
		   {"query.filtered.filter", "{}"},
		   {"query.filtered.filter.range", "{}"},
		   {"query.filtered.filter.range.timestamp", "{}"},
		   {"query.filtered.filter.range.timestamp.gte", TimestampFrom}
		  	
	]),
	case string:str(Function, [<<"aggregate">>]) of %is aggregate the proper name? maybe groupby?
            0 -> %	   	["min" "max" "mean" "???average???" "sum_of_squares" "variance" "std_deviation" ->
		  				 %the following are without aggregation, just statistics   .
						%min, max etc are not added in the query yet, I will investigate if it is even possible
					Facet =  [{"facets", "{}"},
						  	 {"facets.statistics", "{}"},
						  	 {"facets.statistics.statistical", "{}"},
						  	 {"facets.statistics.statistical.script", '(Long) _source.value'}]; %I have to get the value here in another way
			_ -> %		["aggregate", "min" "max" "total" "average"] ->		can also support custom calculations like *,+,-,/, ^(prob)
					% as above
					Facet = [{"facets", "{}"},
							{"facets.tag_price_stats", "{}"},
							{"facets.tag_price_stats.terms_stats", "{}"},
							{"facets.tag_price_stats.terms_stats.key_field", 'timestamp'},
							{"facets.tag_price_stats.terms_stats.value_script", '(Long) _source.value'},
							{"facets.tag_price_stats.terms_stats.size", 100}] %necessary????
	end,
	lib_json:add_values(Query, Facet)
.


%% @doc
%% Function: process_search/3
%% Purpose: Does search for Datapoints for either search done with POST or GET
%% Returns: {true, ReqData, State} || {{error, Reason}, ReqData, State}

%the POST range query should be structed as follows:
%	curl -XPOST http://localhost:8000/streams/1/data/_search -d '{
%		"size" : 100,
%		query:{
% 		   "filtered" : {
% 		       "query" : {
%  		          "term" : { "streamid" : Id }
%  		      }, "filter" : {  "range" : {    "timestamp" : {"gte" : timestampFromValue,"lte" : timestampToValue}}}}
%		 },"sort" : [{"timestamp" : {"order" : "asc"}}]  }'
%the GET range query should be structed as follows:
%	curl -XGET http://localhost:8000/streams/Id/data/_search    --   to return all the datapoints of the current stream
%or	curl -XGET http://localhost:8000/streams/Id/data/_search\?timestampFrom\=timestampFromValue\&timestampTo\=timestampToValue    --   for range query
%or 	curl -XGET http://localhost:8000/streams/Id/data/_search\?timestampFrom\=timestampFromValue   --   for lower bounded only range qquery
%% @end
-spec process_search(ReqData::tuple(), State::string(), term()) ->
		{list(), tuple(), string()}.
process_search(ReqData, State, post) ->
		{Json,_,_} = api_help:json_handler(ReqData,State),
		case erlastic_search:search_json(#erls_params{},?INDEX, "datapoint", Json) of
				{error, Reason} -> {{error,Reason}, wrq:set_resp_body("{\"error\":\""++ atom_to_list(Reason) ++ "\"}", ReqData), State};
				{ok,JsonStruct} ->
						       FinalJson = lib_json:get_list_and_add_id(JsonStruct),
						       {true,wrq:set_resp_body(lib_json:encode(FinalJson),ReqData),State}
		end.


%% @doc
%% Function: generate_date/2
%% Purpose: Used to create a date valid in ES
%%          from the input which should be the list
%%          [Year,Mounth,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_date(DateList::list()) -> string().

generate_date([First]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First);
		false -> "" ++ integer_to_list(First)
	end;
generate_date([First|Rest]) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest);
		false -> "" ++ integer_to_list(First) ++ "-" ++ generate_date(Rest)
	end.
