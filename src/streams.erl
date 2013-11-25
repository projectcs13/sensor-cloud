%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == streams ==
%% This module will contain all functions needed to handle 
%% http requests done to the webmachine regarding streams 
%%
%% @end
-module(streams).
-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
		 delete_resource/2, process_post/2, put_stream/2, get_stream/2,delete_data_points_with_stream_id/1]).


-define(ELASTIC_SEARCH_URL, api_help:get_elastic_search_url()).
-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include("field_restrictions.hrl").

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
%% Purpose: Used to define what methods are allowed one the given URI's.
%% Returns: {List, ReqData, State}, where list is the allowed methods for the given URI. 
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.

allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"streams", _StreamID}, {"_rank"}] ->
			{['PUT'], ReqData, State};
		[{"streams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"streams","_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"streams"}] ->
			{['POST', 'GET'], ReqData, State}; 
		[{"users", _UserID}, {"streams"}] ->
			{['POST', 'GET', 'DELETE'], ReqData, State};
		[{"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[error] ->
		    {[], ReqData, State} 
	end.



%% @doc
%% Function: content_types_provided/2
%% Purpose: based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_provided(ReqData::term(),State::term()) -> {list(), term(), term()}.

content_types_provided(ReqData, State) ->
	{[{"application/json", get_stream}], ReqData, State}.


%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
-spec content_types_accepted(ReqData::term(),State::term()) -> {list(), term(), term()}.

content_types_accepted(ReqData, State) ->
	{[{"application/json", put_stream}], ReqData, State}.



%% @doc
%% Function: delete_resource/2
%% Purpose: Used to handle DELETE requests by deleting the stream in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% FIX: This function relies on direct contact with elastic search at localhost:9200
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

delete_resource(ReqData, State) ->
	case {proplists:get_value('user', wrq:path_info(ReqData)),proplists:get_value('stream', wrq:path_info(ReqData))} of
		{UserId,undefined} ->
			case users:delete_streams_with_user_id(UserId) of
				{error, {Code, Body}} -> 
					ErrorString = api_help:generate_error(Body, Code),
					{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok} ->
					{true,wrq:set_resp_body("{\"message\":\"All streams with user_id:" ++UserId++" are now deleted\"}",ReqData),State}
			end;
		{_,Id} ->
			case delete_data_points_with_stream_id(Id) of 
				{error, {Code, Body}} -> 
					ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
				{ok} ->
					case erlastic_search:delete_doc(?INDEX,"stream", Id) of
						{error, {Code, Body}} -> 
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} -> 
			 				{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
					end
			end
	end.

%% @doc
%% Function: delete_data_points_with_stream_id/1
%% Purpose: Used to delete all data-points with the given id as parent
%% Returns: {ok} or {error,Reason} 
%% FIX: This function relies on direct contact with elastic search at localhost:9200
%% @end
-spec delete_data_points_with_stream_id(Id::string() | binary()) -> term().

delete_data_points_with_stream_id(Id) when is_binary(Id) ->
	{ok, {{_Version, Code, _ReasonPhrase}, _Headers, Body}} = httpc:request(delete, {"http://localhost:9200/sensorcloud/datapoint/_query?q=stream_id:" ++ binary_to_list(Id), []}, [], []),
	case Code of
		200 ->
			{ok};
		Code ->
			{error,{Code, Body}}
	end;
delete_data_points_with_stream_id(Id) ->
	{ok, {{_Version, Code, _ReasonPhrase}, _Headers, Body}} = httpc:request(delete, {"http://localhost:9200/sensorcloud/datapoint/_query?q=stream_id:" ++ Id, []}, [], []),
	case Code of
		200 ->
			{ok};
		Code ->
			{error,{Code, Body}}
	end.

%% @doc
%% Function: process_post/2
%% Purpose: Used to handle POST requests by creating streams, or search for streams in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the post request is
%% successful and false otherwise.
%% @end
-spec process_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_post(ReqData, State) ->
	case api_help:is_search(ReqData) of 
		false ->
			{Stream,_,_} = api_help:json_handler(ReqData, State),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserAdded = Stream;
				UId ->
					UserAdded = api_help:add_field(Stream,"user_id",UId)
			end,
			case lib_json:get_field(UserAdded,"user_id") of
				undefined -> {false, wrq:set_resp_body("\"user_id missing\"",ReqData), State};
				UserId ->
					case {api_help:do_any_field_exist(UserAdded,?RESTRCITEDCREATESTREAMS),api_help:do_only_fields_exist(UserAdded,?ACCEPTEDFIELDSSTREAMS)} of
						{true,_} ->
							ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRCITEDCREATESTREAMS),
							ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
							{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
						{false,false} ->
							{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
						{false,true} ->
			%				case erlastic_search:get_doc(?INDEX, "user", UserId) of
			%					{error,{404,_}} ->
			%						{{halt,403}, wrq:set_resp_body("{\"error\":\"no document with resource_id given is present in the system\"}", ReqData), State};
			%					{error,{Code,Body}} ->
			%						ErrorString = api_help:generate_error(Body, Code),
            %						{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
			%					{ok,_} ->
									FieldsAdded = add_server_side_fields(UserAdded),
									%Final = suggest:add_stream_suggestion_fields(FieldsAdded),
									case erlastic_search:index_doc(?INDEX, "stream", FieldsAdded) of	
										{error,{Code,Body}} ->
											ErrorString = api_help:generate_error(Body, Code),
            								{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
										{ok,List} -> 
											%suggest:update_suggestion(UserAdded),
											{true, wrq:set_resp_body(lib_json:encode(List), ReqData), State}
									end
			%				end
					end
			end;
		true ->
			process_search_post(ReqData,State)	
	end.


%% @doc
%% Function: process_search_post/2
%% Purpose: Used to handle search requests that come from POST requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_post(ReqData, State) ->
        case wrq:get_qs_value("size",ReqData) of 
            undefined ->
                Size = "10";
            SizeParam ->
                Size = SizeParam
        end,
        case wrq:get_qs_value("from",ReqData) of
            undefined ->
                From = "0";
            FromParam ->
                From = FromParam
        end,
        {Json,_,_} = api_help:json_handler(ReqData,State),
        case proplists:get_value('user', wrq:path_info(ReqData)) of
                undefined ->
                        FilteredJson = filter_json(Json, From, Size);
                UserId ->
                        ResQuery = "\"user_id\":" ++ UserId,
                        FilteredJson = filter_json(Json, ResQuery, From, Size)
        end,
        case erlastic_search:search_json(#erls_params{},?INDEX, "stream", FilteredJson) of % Maybe wanna take more
                {error, {Code, Body}} -> 
					ErrorString = api_help:generate_error(Body, Code),
            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
                {ok,List} -> 
					{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State} % May need to convert
        end.


%% @doc
%% Function: process_search_get/2
%% Purpose: Used to handle search requests that come from GET requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_get(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_get(ReqData, State) ->
	URIQuery = wrq:req_qs(ReqData),
    case wrq:get_qs_value("size",ReqData) of 
        undefined ->
            Size = 100;
        SizeParam ->
            Size = list_to_integer(SizeParam)
    end,
	case proplists:get_value('user', wrq:path_info(ReqData)) of
		undefined ->
			UserQuery = [],
			UserDef = false;
		UserId ->
			UserQuery = "user_id:" ++ UserId,
			UserDef = true
		end,
	FullQuery = lists:append(api_help:transform(URIQuery,UserDef),UserQuery),
	case erlastic_search:search_limit(?INDEX, "stream", FullQuery,Size) of % Maybe wanna take more
		{error, {Code, Body}} -> 
			ErrorString = api_help:generate_error(Body, Code),
            {{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
		{ok,List} -> 
			{lib_json:encode(List),ReqData,State} 
	end.


%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updating the given documents in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

put_stream(ReqData, State) ->
	case api_help:is_rank(ReqData) of
		false ->
			StreamId = proplists:get_value('stream', wrq:path_info(ReqData)),
			{Stream,_,_} = api_help:json_handler(ReqData,State),
			case {api_help:do_any_field_exist(Stream,?RESTRCITEDUPDATESTREAMS),api_help:do_only_fields_exist(Stream,?ACCEPTEDFIELDSSTREAMS)} of
				{true,_} -> 
					ResFields1 = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, "", ?RESTRCITEDUPDATESTREAMS),
					ResFields2 = string:sub_string(ResFields1, 1, length(ResFields1)-2),
					{{halt,409}, wrq:set_resp_body("{\"error\":\"Error caused by restricted field in document, these fields are restricted : " ++ ResFields2 ++"\"}", ReqData), State};
				{false,false} ->
					{{halt,403}, wrq:set_resp_body("Unsupported field(s)", ReqData), State};
				{false,true} ->
					%NewJson = suggest:add_stream_suggestion_fields(Stream),
					%Update = api_help:create_update(NewJson),
					Update = api_help:create_update(Stream),
					%suggest:update_stream(Stream, StreamId),
					case api_help:update_doc(?INDEX, "stream", StreamId, Update) of 
						{error, {Code, Body}} -> 
							ErrorString = api_help:generate_error(Body, Code),
		            		{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} -> 
							{true,wrq:set_resp_body(lib_json:encode(List),ReqData),State}
					end
			end;
		true ->
			erlang:display("IN RANK!"),
			StreamId = proplists:get_value('stream', wrq:path_info(ReqData)),
			case wrq:get_qs_value("ranking",ReqData) of 
				undefined ->
                	Rank = {error, {<<"{\"error\":\"Error, incorrect or no ranking specified.\"}">>, 409}};
				Ranking ->
					
					case string:to_float(Ranking) of
						{Number,_} when (Number >= 0.0) and (Number =< 100.0) ->
							Rank = Number;
	 					_ -> 
                			Rank = {error, {<<"{\"error\":\"Error, incorrect or no ranking specified.\"}">>, 409}}
                	end
        	end,
        	case wrq:get_qs_value("user_id",ReqData) of 
            undefined ->
                User = {error, {<<"{\"error\":\"Error, no user specified.\"}">>, 409}};
            UserId ->
                User = UserId
        	end,
        	case Rank of
        		{error, {Body, Code}} ->
        			ErrorString = api_help:generate_error(Body, Code),
        			{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
        		_ -> 
	        		case User of
	          			{error, {Body, Code}} ->
	            			ErrorString = api_help:generate_error(Body, Code),
	            			{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
	          			_ ->

	          			case erlastic_search:get_doc(?INDEX,"user", User) of
	          				{error, {Code, Body}} ->
	            				ErrorString = api_help:generate_error(Body, Code),
	            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
	            			{ok,List} ->
	            				case lib_json:get_field(List, "_source.rankings") of
	            					undefined ->
	            						change_ranking(StreamId, Rank),
	            						UpdateJson = "{\"script\" : \"ctx._source.rankings += ranking\",\"params\":{\"ranking\":{ \"rank\":"++ float_to_list(Rank) ++",\"stream_id\":\""++StreamId++"\"}}}",
	            						case api_help:update_doc(?INDEX, "user", User, UpdateJson, []) of
											{error, {Code, Body}} ->
					            				ErrorString = api_help:generate_error(Body, Code),
					            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
					            			{ok, List2} -> {true,wrq:set_resp_body(lib_json:encode(List2),ReqData),State}
					            		end;
	            					RankingList ->
	            						case find_ranking(StreamId, RankingList, Rank, []) of
	            							not_found ->
	            								change_ranking(StreamId, Rank),
	            								UpdateJson = "{\"script\" : \"ctx._source.rankings += ranking\",\"params\":{\"ranking\":{ \"rank\":"++ float_to_list(Rank) ++",\"stream_id\":\""++StreamId++"\"}}}",
		            							case api_help:update_doc(?INDEX, "user", User, UpdateJson,[]) of
													{error, {Code, Body}} ->
						            					ErrorString = api_help:generate_error(Body, Code),
						            					{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						            				{ok, List3} -> {true,wrq:set_resp_body(lib_json:encode(List3),ReqData),State}
					            				end;
	            							{OldRank, ChangedRankingList} ->
	                							change_ranking(StreamId, Rank, OldRank),
	                							UpdateJson = api_help:create_update(lib_json:set_attr("rankings", ChangedRankingList)),
	            								case api_help:update_doc(?INDEX, "user", User, UpdateJson,[]) of
													{error, {Code, Body}} ->
						            					ErrorString = api_help:generate_error(Body, Code),
						            					{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						            				{ok, List4} -> {true,wrq:set_resp_body(lib_json:encode(List4),ReqData),State}
					            				end
	            						end
	            				end
	            		end
	        		end
    		end
	end.




%% @doc
%% Function: get_stream/2
%% Purpose: Used to handle GET requests by giving the document with the given
%% Id or listing the documents that can be found from the restrictions
%% given by the URI.
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec get_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.


get_stream(ReqData, State) ->
	case api_help:is_search(ReqData) of
		true -> process_search_get(ReqData,State);
		false ->
			case proplists:get_value('stream', wrq:path_info(ReqData)) of
				undefined ->
				% List streams based on URI
			        case wrq:get_qs_value("size",ReqData) of 
			            undefined ->
			                Size = 100;
			            SizeParam ->
			                Size = list_to_integer(SizeParam)
			        end,
					case proplists:get_value('user', wrq:path_info(ReqData)) of
						undefined ->
							UserQuery = [],
							UserDef = false;
						UserId ->
							UserQuery = "\"user_id\":\"" ++ UserId ++ "\"",
							UserDef = true
					end,
					case UserDef of
						true -> 
							Query = "{\"size\" :" ++ integer_to_list(Size) ++",\"query\" : {\"term\" : {" ++ UserQuery ++ "}}}";
						false -> 
							Query = "{\"size\" :" ++ integer_to_list(Size) ++",\"query\" : {\"match_all\" : {}},\"filter\" : {\"bool\":{\"must_not\":{\"term\":{\"private\":\"true\"}}}}}"
					end,  
					case erlastic_search:search_json(#erls_params{},?INDEX, "stream", Query) of % Maybe wanna take more
						{error, {Code, Body}} -> 
            				ErrorString = api_help:generate_error(Body, Code),
            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
					    {ok,JsonStruct} ->
						    FinalJson = lib_json:get_list_and_add_id(JsonStruct, streams),
						    {FinalJson, ReqData, State}
					end;
				StreamId ->
				% Get specific stream
					case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
						{error, {Code, Body}} -> 
            				ErrorString = api_help:generate_error(Body, Code),
            				{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,JsonStruct} -> 	 
						    FinalJson = lib_json:get_and_add_id(JsonStruct),
						    {FinalJson, ReqData, State}
					end
				end
	end.

%% @doc
%% Function: filter_json/1
%% Purpose: Used to add private filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must_not\":{\"term\":{\"private\":\"true\"}}}}}}}".

%% @doc
%% Function: filter_json/2
%% Purpose: Used to add private and resource filters to the json query
%% Returns: JSON string that is updated with filter
%% @end
filter_json(Json,ResourceQuery) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must_not\":[{\"term\":{\"private\":\"true\"}},{\"term\":{"++ResourceQuery++"}}]}}}}}".



%% @doc
%% Function: filter_json/3
%% Purpose: Used to add private filters to the json query with pagination
%% Returns: JSON string that is updated with filter and the from size parameters
%% @end
filter_json(Json, From, Size) ->
        NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"from\" : "++From++", \"size\" : "++Size++", \"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must_not\":{\"term\":{\"private\":\"true\"}}}}}}}".


%% @doc
%% Function: filter_json/4
%% Purpose:  Used to add private and resource filters to the json query with pagination
%% Returns: JSON string that is updated with filter and the from size parameters
%% @end
filter_json(Json, ResourceQuery, From, Size) ->
         NewJson = string:sub_string(Json,1,string:len(Json)-1),
        "{\"from\" : "++From++", \"size\" : "++Size++", \"query\":{\"filtered\":"++NewJson++",\"filter\":{\"bool\":{\"must_not\":[{\"term\":{\"private\":\"true\"}},{\"term\":{"++ResourceQuery++"}}]}}}}}".



%% @doc
%% Function: find_ranking/1
%% Purpose: Used to find a ranking value in a list of rankings(list of JSON objects) 
%% based on a stream_id
%%         
%% Returns: The value of the rank that relates to the stream_id in the provided list
%% @end
find_ranking(StreamId, Rankings, NewRank, List) when is_list(StreamId) ->
	find_ranking(binary:list_to_bin(StreamId), Rankings, NewRank, []);
find_ranking(StreamId, [], NewRank, List) ->
	not_found;
find_ranking(StreamId, [Head|Rest], NewRank ,List) ->
	case lib_json:get_field(Head, "stream_id") of 
		StreamId ->
			OldRank = lib_json:get_field(Head, "rank"),
			ChangedJson = lib_json:replace_field(Head, "rank", NewRank),
			{OldRank, lists:append([List,[ChangedJson],Rest])};
		_ ->
			find_ranking(StreamId, Rest, NewRank, lists:append(List,[Head]))
	end.
		


%% @doc
%% Function: change_ranking/2
%% Purpose: Used to add a new ranking value to a stream 
%%          in ES
%%
%% @end
-spec change_ranking(StreamId::string(), NewValue::float()) -> ok | {error, no_model}. 

change_ranking(StreamId, NewValue) ->
	case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
		{error, {Code, Body}} -> {error, {Code, Body}};
		{ok,JsonStruct} -> 	 
		erlang:display(lib_json:get_field(JsonStruct, "_source.user_ranking.average")),
				OldRanking = float(lib_json:get_field(JsonStruct, "_source.user_ranking.average")),
				NumberOfRankings = lib_json:get_field(JsonStruct, "_source.user_ranking.nr_rankings"),
				NewNumberOfRankings = NumberOfRankings + 1,
				NewRanking = ((OldRanking * NumberOfRankings) + NewValue) / NewNumberOfRankings,
				RankingJson = lib_json:set_attrs( [{user_ranking, "{}"}, {"user_ranking.average", NewRanking}, {"user_ranking.nr_rankings", NewNumberOfRankings} ] ),
				case api_help:update_doc(?INDEX, "stream",StreamId, api_help:create_update(RankingJson),[]) of 
					{error, {Code, Body}} -> {error, {Code, Body}};
					{ok, _} -> 	ok
				end
	end.

-spec change_ranking(StreamId::string(), NewValue::float(), OldValue::float()) -> ok | {error, no_model}. 

change_ranking(StreamId, NewValue, OldValue) ->
case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
		{error, {Code, Body}} -> {error, {Code, Body}};
		{ok,JsonStruct} -> 	 
				OldRanking = float(lib_json:get_field(JsonStruct, "_source.user_ranking.average")),
				NumberOfRankings = lib_json:get_field(JsonStruct, "_source.user_ranking.nr_rankings"),
				NewRanking = ((OldRanking * NumberOfRankings) + NewValue - OldValue) / NumberOfRankings,
				RankingJson = lib_json:set_attrs( [{user_ranking, "{}"}, {"user_ranking.average", NewRanking}, {"user_ranking.nr_rankings", NumberOfRankings} ] ),
				case api_help:update_doc(?INDEX, "stream",StreamId, api_help:create_update(RankingJson),[]) of 
					{error, {Code, Body}} -> {error, {Code, Body}};
					{ok, _} -> 	ok
				end
	end.

		
%% @doc
%% Function: add_server_side_fields/1
%% Purpose: Used to add all the fields that should be added server side
%% Returns: The new json with the fields added
%% @end
-spec add_server_side_fields(Json::string()) -> string().

add_server_side_fields(Json) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:local_time(),
	Date = api_help:generate_date([Year,Month,Day]),

	Time = api_help:generate_timestamp([Year,Month,Day,Hour,Min,Sec],0),

	JSON = lib_json:add_values(Json,[
			{creation_date, list_to_binary(Date)},
			{last_updated, list_to_binary(Time)},
			{quality, 1.0},
			{subscribers, 1},
			{history_size, 0}, 
			{user_ranking, "{}"},
			{"user_ranking.average", 0.0},
			{"user_ranking.nr_rankings", 0},
			{active, true}]),
	erlang:display(JSON),
	JSON.


%%	DateAdded = api_help:add_field(Json,"creation_date",Date),
%%	UpdateAdded = api_help:add_field(DateAdded,"last_update",Time),
%%	QualityAdded = api_help:add_field(UpdateAdded,"quality",1.0),
%%	UserRankingAdded = api_help:add_field(QualityAdded,"user_ranking","{}"),
%%	UserRankingAverageAdded = api_help:add_field(UserRankingAdded,"user_ranking.average",0.0),
%%	UserRankingNrRankingsAdded = api_help:add_field(UserRankingAverageAdded,"user_ranking.nr_rankings",0),
%%	SubsAdded = api_help:add_field(UserRankingNrRankingsAdded,"subscribers",1),
%%	api_help:add_field(SubsAdded,"history_size",0).

