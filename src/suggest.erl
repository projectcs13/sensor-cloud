%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @headerfile "json.hrl"
%% @copyright [Copyright information]
%% @doc == Module for creating, updating and retrieving suggestions ==
%% @end
-module(suggest).
-export([init/1, 
	allowed_methods/2, 
	content_types_provided/2, 
	get_suggestion/2, 
	add_suggestion/2,
	update_suggestion/1,
	update_stream/2,
	update_resource/2,
	add_resource_suggestion_fields/1,
	add_stream_suggestion_fields/1
	]).


-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include("json.hrl").

-define(INDEX, "sensorcloud").



%% @doc
%% Init function used to fetch path information from webmachine dispatcher.
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
	{ok, undefined}.

%% @doc
%% Define what methods are allowed one the given URI's. It is called automatically by webmachine
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.
allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"suggest", "_search"}] ->
			{['GET'], ReqData, State};
		[{"suggest", _Field} , {_Term}] ->
			{['GET'], ReqData, State}; 
		[{"suggest", _Term}] ->
			{['GET'], ReqData, State}; 
		[error] ->
			{[], ReqData, State} 
	end.



%% @doc
%% Based on the Accept header on a 'GET' request, we provide different media types to the client.
%% A code 406 is returned to the client if we cannot return the media-type that the user has requested.
%% @end
-spec content_types_provided(ReqData::term(),State::term()) -> {list(), term(), term()}.
content_types_provided(ReqData, State) ->
	{[{"application/json", get_suggestion}], ReqData, State}.



%% @doc
%% Handles suggestion request.
%%
%% Case 1:
%% Handles GET requests for suggestions by giving the term.(model). It returns only one suggestion,
%% the one with the highest score.
%%
%% Example URL: localhost:8000/suggest/my_model 
%%
%% Case 2:
%% Handles GET request for text autocompletion.
%%
%% Example URL: localhost:8000/suggest/my_field/my_text?size=5 
%%
%% Case 3:
%% Handles GET requests for phrase_suggestion/auto_completion
%%
%% Example URL: localhost:8000/suggest/_search?query=test
%% This will try to find completion suggestions for the query "test"
%% @end
-spec get_suggestion(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
get_suggestion(ReqData, State) ->
	case api_help:is_search(ReqData) of
		false ->
			case wrq:get_qs_value("size",ReqData) of 
				undefined ->
					Size = "1";
				SizeParam ->
					Size = SizeParam
			end,
			case proplists:get_value('field', wrq:path_info(ReqData)) of
				undefined ->
					Field = "suggest";
				FieldParam ->
					Field = FieldParam ++ "_suggest"
			end,
			case proplists:get_value('term', wrq:path_info(ReqData)) of
				undefined ->
					{{halt, 400}, ReqData, State};
				Term ->
					Query = "{                   
							\"testsuggest\" : {     
								\"text\" : \""++http_uri:decode(Term)++"\",
								\"completion\" : {                    
								\"field\" : \""++Field++"\",
										\"size\" : "++ Size ++"            
								}                                                   
							}                                      
						}",
					case erlastic_search:suggest(?INDEX, Query) of	
						{error, {Code, Body}} -> 
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} -> 
							EncodedList = lib_json:encode(List),
							case re:run(EncodedList, "\"options\":\\[\\]", [{capture, first, list}]) of
								{match, _} -> 
									Output = lib_json:set_attr("suggestions",[]);
								_-> 
									Output = lib_json:set_attr("suggestions",lib_json:get_field(List, "testsuggest[0].options")),
																	erlang:display(Output)

							end,
							{lib_json:encode(Output),ReqData, State}
					end
			end;
		true ->
			case wrq:get_qs_value("query",ReqData) of
				undefined ->
					erlang:display("No query specified!");
				QueryString ->
					SuggestJson = "{\"suggestion\":{\"text\":\""++ QueryString ++"\",\"completion\":{\"field\":\"search_suggest\",\"fuzzy\":true}}}",
					case erlastic_search:suggest(?INDEX, SuggestJson) of
						{error, {Code, Body}} -> 
							ErrorString = api_help:generate_error(Body, Code),
							{{halt, Code}, wrq:set_resp_body(ErrorString, ReqData), State};
						{ok,List} ->
							OutputJson = lib_json:set_attr("suggestions",lib_json:get_field(List, "suggestion[0].options")),
							{OutputJson ,ReqData ,State}
					end
			end
	end.




%% @doc
%% Creates a suggestion using a resource. This new suggestion contains only the metadata from the resource
%% like manufacturer, tags, polling_frequency. It is expected to be updated with information about the
%% streams when new streams are created for that resource. 
%% @end
-spec add_suggestion(Resource::json(), RsourceId::binary()) -> ok | {error, no_model}. 
add_suggestion(Resource, ResourceId) ->
	Manufacturer = lib_json:get_field(Resource, "manufacturer"),
	Model = lib_json:get_field(Resource, "model"),
	Tags = lib_json:get_field(Resource, "tags"),
	Polling_freq = lib_json:get_field(Resource, "polling_freq"),
	Weight = scoring:calc(Resource, resource),
	case Model of 
		undefined ->
			{error, no_model};
		_ ->
			Suggestion = lib_json:set_attrs(
						  [
						   {resource_id, ResourceId},
						   {suggest, "{}"},
						   {"suggest.input", [Model, Manufacturer, binary:list_to_bin(binary_to_list(Manufacturer) ++ " " ++ binary_to_list(Model))]},
						   {"suggest.output", Model},
						   {"suggest.payload", "{}"},
						   {"suggest.payload.resource", ResourceId},
						   {"suggest.payload.model", Model},
						   {"suggest.payload.manufacturer", Manufacturer},
						   {"suggest.weight", Weight}
						  ]
						 ),
			case erlastic_search:index_doc(?INDEX, "suggestion", Suggestion) of 
				{error, _Reason} -> erlang:display("Suggestion not saved ");
				{ok, _} -> 	ok
			end
	end.


%% @doc
%% When inserting or updating a resource, it generates the suggest fields
%% @end
add_resource_suggestion_fields(Resource) ->
	case lib_json:get_field(Resource,"manufacturer") of
		undefined ->
			Temp1 = Resource;
		Manufacturer ->
			Temp1 = lib_json:add_value(Resource, "manufacturer_suggest", Manufacturer)
	end,
	case lib_json:get_field(Resource,"model") of
		undefined ->
			Temp2 = Temp1;
		Model ->
			Temp2 = lib_json:add_value(Temp1, "model_suggest", Model)
	end,
	case lib_json:get_field(Resource,"tags") of
		undefined ->
			Temp3 = Temp2;
		Tags ->
			Temp3 = lib_json:add_value(Temp2, "tags_suggest", Tags)
	end,
	Temp3.

%% @doc
%% When inserting or updating a stream, it generates the suggest fields
%% @end
add_stream_suggestion_fields(Stream) ->
	case lib_json:get_field(Stream,"name") of
		undefined ->
			Temp1 = Stream;
		Name ->
			Temp1 = lib_json:add_value(Stream, "name_suggest", Name)
	end,
	case lib_json:get_field(Stream,"type") of
		undefined ->
			Temp2 = Temp1;
		Type ->
			Temp2 = lib_json:add_value(Temp1, "type_suggest", Type)
	end,
	case lib_json:get_field(Stream,"tags") of
		undefined ->
			Temp3 = Temp2;
		Tags ->
			Temp3 = lib_json:add_value(Temp2, "tags_suggest", Tags)
	end,
	Temp3.


%% @doc
%% Updates the suggestion to include information from the new stream. This way we can later
%% on autocomplete the number of streams for that resource, along with some more information 
%% about each stream. 
%% @end
-spec update_suggestion(Stream::json()) -> ok.
update_suggestion(Stream) ->
	
	%% only for testing
	erlang:display("run function: update_suggestion/1"),
	
	ResourceId = lib_json:get_field(Stream, "resource_id"),
	case erlastic_search:search(?INDEX, "suggestion", "resource_id:"++ lib_json:to_string(ResourceId)) of
		{error, _} -> erlang:display("ERROR");
		{ok, Response} ->
			case lib_json:get_field(Response, "hits.hits[0]._source.resource_id") of
				ResourceId ->
					Weight = lib_json:get_field(Response, "hits.hits[0]._source.suggest.weight"),
					Id = lib_json:get_field(Response, "hits.hits[0]._id"),
					Payload = lib_json:get_field(Response, "hits.hits[0]._source.suggest.payload"),
					{AddWeight, StreamInfo} = get_stream_info(Stream),
					NewWeight = Weight + AddWeight,
					Sugg = lib_json:get_field(Response, "hits.hits[0]._source"),
					case lib_json:get_field(Response, "hits.hits[0]._source.suggest.payload.streams") of
						undefined ->
							NewPayload = lib_json:add_value(Payload, "streams", "["++StreamInfo++"]"),

							TempSugg = lib_json:replace_field(Sugg, "suggest.payload", lib_json:to_string(NewPayload)),
							NewSugg = lib_json:replace_field(TempSugg, "suggest.weight", NewWeight);
						_OldStream ->

							NewStreamList = lib_json:add_value(Sugg,"suggest.payload.streams" , StreamInfo),
							NewSugg = lib_json:replace_field(NewStreamList, "suggest.weight", NewWeight)
					end,
					Final = lib_json:set_attr(doc,NewSugg),
					case api_help:update_doc(?INDEX, "suggestion", Id, Final) of 
						{error, _Reason} -> erlang:display("not updated");
						{ok, _Json} -> 
							erlang:display("succeed updating the suggestion!!"),
							ok 
					end;
				_ -> 
					erlang:display("No suggestion exists for that resource")
			end
	end.

%% @doc
%% Updates the suggestion to reflect the changes that has been done in a resource.
%% @end
-spec update_resource(Resource::json(), ResourceId::string()) -> ok.
update_resource(Resource, ResourceId) ->
	Manufacturer = lib_json:get_field(Resource, "manufacturer"),
	Model = lib_json:get_field(Resource, "model"),
	Tags = lib_json:get_field(Resource, "tags"),
	Polling_freq = lib_json:get_field(Resource, "polling_freq"),
	RId = list_to_binary(ResourceId),
	%fetch old suggestion
	case erlastic_search:search(?INDEX, "suggestion", "resource_id:"++ ResourceId) of
		{error, _} -> erlang:display("ERROR");
		{ok, Response} ->
			case lib_json:get_field(Response, "hits.hits[0]._source.resource_id") of
				RId ->
					SuggId = lib_json:get_field(Response, "hits.hits[0]._id"),
					Json = lib_json:get_field(Response, "hits.hits[0]._source"), 
					UpdatedJson = lib_json:replace_fields(Json, [{"suggest.payload.manufacturer",Manufacturer},{"suggest.payload.model",Model},{"suggest.payload.tags",Tags},{"suggest.payload.pollng_feq",Polling_freq}]),
					WeightJson = update_score(UpdatedJson),
					%change input (in case model changed)
					FinalJson = lib_json:replace_field(WeightJson, "suggest.input",Model),
					case erlastic_search:index_doc_with_id(?INDEX, "suggestion", SuggId, FinalJson) of 
						{error, _Reason} -> erlang:display("Suggestion not saved ");
						{ok, _} -> 	ok
					end;
				_ -> 
					erlang:display("No suggestion exists for that resource")
			end
	end,
	ok.


%% @doc
%% Updates the suggestion to reflect the changes that has been done in a stream.
%% @end
-spec update_stream(Stream::json(), StreamId::string()) -> ok.
update_stream(Stream, StreamId) ->
	%fetch old values so that we can replace them
	case erlastic_search:get_doc(?INDEX, "stream", StreamId) of
		{error, _Reason} -> erlang:display("Stream not found");
		{ok, OldStreamJson} ->
			{_, OldStream} = get_stream_info(lib_json:get_field(OldStreamJson, "_source")),
			{_, NewStream} = get_stream_info(Stream),
			ResourceId = lib_json:get_field(OldStreamJson, "_source.resource_id"),
			case ResourceId of
				undefined ->
					ok;
				_ ->
					%fetch old suggestion
					case erlastic_search:search(?INDEX, "suggestion", "resource_id:" ++ binary_to_list(ResourceId)) of
						{error, _Reason2} -> erlang:diplay("Suggestion not found :S");
						{ok, OldSuggestion} -> 
							StreamList = lib_json:get_field(OldSuggestion, "hits.hits[0]._source.suggest.payload.streams"),
							case StreamList of
								undefined ->
									ok;
								_ ->
									case string:str(StreamList, [OldStream]) of
										0 -> erlang:display("Invalid position. Not matched properly");
										Pos ->
											SuggId = lib_json:get_field(OldSuggestion, "hits.hits[0]._id"),
											Suggestion = lib_json:get_field(OldSuggestion, "hits.hits[0]._source"),
											UpdatedSuggestion = lib_json:replace_field(Suggestion, lists:concat(["suggest.payload.streams[", Pos-1, "]"]), NewStream),
											FinalSuggestion = update_score( UpdatedSuggestion),
											case erlastic_search:index_doc_with_id(?INDEX, "suggestion", SuggId, FinalSuggestion) of 
												{error, _Reason} -> erlang:display("Suggestion not updated ");
												{ok, _} -> 	ok
											end
									end
							end
					end
			end
	end,
	ok.



%% @doc
%% Updates the weight of the suggestion after the new information has been added to it
%% It takes into account both resource and stream.
%% @end
-spec update_score(Suggestion::json()) -> json().
update_score(Suggestion) ->
	Payload = lib_json:get_field(Suggestion, "suggest.payload"),
	ResourceWeight = scoring:calc(Payload, resource),
	Streams = lib_json:get_field(Payload, "streams"),
	case Streams of
		undefined ->
			StreamWeight = 0;
		_ -> 
			Fun = fun(Stream, Acc) -> 
					scoring:calc(Stream,stream)+Acc
			end,
			StreamWeight = lists:foldr(Fun, 0, Streams)
	end,
	Sum = ResourceWeight + StreamWeight,
	lib_json:replace_field(Suggestion, "suggest.weight", Sum).




%% @doc
%% It keeps usefull information for the given stream. It forms a new json object using only these 
%% information and also returns the difference on the scoring of the suggestion
%% @end
-spec get_stream_info(Stream::json()) -> {Weight::integer(), Result::json_string()}.
get_stream_info(Stream) ->
	Name = lib_json:get_field(Stream, "name"),
	Description = lib_json:get_field(Stream, "description"),
	Min_val  = lib_json:get_field(Stream, "min_val"),
	Max_val  = lib_json:get_field(Stream, "max_val"),
	Tags  = lib_json:get_field(Stream, "tags"),
	Type  = lib_json:get_field(Stream, "type"),
	Accuracy  = lib_json:get_field(Stream, "accuracy"),
	Weight = scoring:calc([Name, Description, Min_val, Max_val, Tags, Type, Accuracy]),
	Result = lib_json:set_attrs([{name, Name},
				{description, Description},
				{min_value, Min_val},
				{max_value, Max_val},
				{tags, Tags},
				{type, Type},
				{accuracy, Accuracy}
				]),
	{Weight, Result}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Should be moved to own module later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% Returns the current timestamp.
%% @end
get_timestamp() ->
	TS = {_MSec,_Sec,Micro} = os:timestamp(),
	{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
	Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
	binary:list_to_bin(io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day,Mstr,Year,Hour,Minute,Second,Micro])).




