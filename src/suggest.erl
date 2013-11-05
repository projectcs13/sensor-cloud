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

	update_resource/2
	]).


-include_lib("erlastic_search.hrl").
-include("webmachine.hrl").
-include("json.hrl").

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
%% Purpose: Used to define what methods are allowed one the given URI's.
%% Returns: {List, ReqData, State}, where list is the allowed methods for the given URI. 
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.

allowed_methods(ReqData, State) ->
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"suggest", _Term}] ->
			{['GET'], ReqData, State}; 
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
	{[{"application/json", get_suggestion}], ReqData, State}.



%% @doc
%% Handles GET requests for suggestions by giving the term.(model). It returns only one suggestion,
%% the one with the highest score.
%%
%% Example URL: localhost:8000/suggest/my_model 
%% @end
-spec get_suggestion(ReqData::term(),State::term()) -> {boolean(), term(), term()}.
get_suggestion(ReqData, State) ->
	case proplists:get_value('term', wrq:path_info(ReqData)) of
		undefined ->
			{{halt, 400}, ReqData, State};
		Term ->
			%forms the query
			Query = "{                   
					\"testsuggest\" : {     
						\"text\" : \""++Term++"\",
						\"completion\" : {                    
						\"field\" : \"suggest\",
								\"size\" : 1            
						}                                                   
					}                                      
				}",
			case erlastic_search:suggest(?INDEX, Query) of	
				{error, Reason} -> {lib_json:encode(Reason),ReqData, State};
				{ok,List} -> 
					EncodedList = lib_json:encode(List),
					case re:run(EncodedList, "\"options\":\\[\\]", [{capture, first, list}]) of
						{match, _} -> 
							{{halt,404},ReqData, State};
						_->
							{lib_json:encode(List),ReqData, State}
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
			Suggestion = "{
				\"resource_id\" : \"" ++ undefined_to_string(ResourceId) ++ "\",
				\"suggest\" : {
					\"input\" : [ \"" ++ undefined_to_string(Model) ++ "\" ], 
					\"output\" : \"" ++ get_timestamp() ++ "\",
					\"payload\" : { 
						\"manufacturer\" : \"" ++ undefined_to_string(Manufacturer) ++ "\",
						\"tags\" : \"" ++ undefined_to_string(Tags) ++ "\",
						\"polling_freq\" : \"" ++ undefined_to_string(Polling_freq) ++ "\"
					},
					\"weight\" : " ++ integer_to_list(Weight) ++ "
				}				
				}",
			case erlastic_search:index_doc(?INDEX, "suggestion", Suggestion) of 
				{error, _Reason} -> erlang:display("Suggestion not saved ");
				{ok, _} -> 	ok
			end
	end.


%% @doc
%% Updates the suggestion to include information from the new stream. This way we can later
%% on autocomplete the number of streams for that resource, along with some more information 
%% about each stream. 
%% @end
-spec update_suggestion(Stream::json()) -> ok.
update_suggestion(Stream) ->
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
					Final = api_help:create_update(NewSugg),
					case api_help:update_doc(?INDEX, "suggestion", Id, Final) of 
						{error, _Reason} -> erlang:display("not updated");
						{ok, _Json} -> ok 
					end;
				_ -> 
					erlang:display("No suggestion exists for that resource")
			end
	end.

update_resource(Resource, ResourceId) ->
	Manufacturer = undefined_to_string(lib_json:get_field(Resource, "manufacturer")),
	Model = undefined_to_string(lib_json:get_field(Resource, "model")),
	Tags = undefined_to_string(lib_json:get_field(Resource, "tags")),
	Polling_freq = undefined_to_string(lib_json:get_field(Resource, "polling_freq")),
	%fetch old suggestion
	case erlastic_search:search(?INDEX, "suggestion", "resource_id:"++ lib_json:to_string(ResourceId)) of
		{error, _} -> erlang:display("ERROR");
		{ok, Response} ->
			case lib_json:get_field(Response, "hits.hits[0]._source.resource_id") of
				ResourceId ->
					%If suggestion found
					SuggId = lib_json:get_field(Response, "hits.hits[0]._id"),
					Json = lib_json:get_field(Response, "hits.hits[0]._source"), 
					%Payload = lib_json:get_field(Response, "hits.hits[0]._source.suggest.payload"),
					NewJson = lib_json:replace_fields(Json, [{"suggest.payload.manufacturer",Manufacturer},{"suggest.payload.model",Model},{"suggest.payload.tags",Tags},{"suggest.payload.pollng_feq",Polling_freq}]),

					%Delete old suggestion
					erlastic_search:delete_doc("sensorcloud", "suggestion", SuggId),
					%update weight

					%change input (in case model changed)

					%case erlastic_search:index_doc(?INDEX, "suggestion", Suggestion) of 
					%	{error, _Reason} -> erlang:display("Suggestion not saved ");
					%	{ok, _} -> 	ok
					%end,
					%Final = api_help:create_update(Sugg),
					%case api_help:update_doc(?INDEX, "suggestion", SuggId, Final) of 
					%	{error, _Reason} -> erlang:display("not updated");
					%	{ok, _Json} -> ok 
					%end;
					ok;
				_ -> 
					erlang:display("No suggestion exists for that resource")
			end
	end,
	ok.


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
	Result ="{
		\"name\":\"" ++ undefined_to_string(Name)++"\",
		\"description\":\"" ++ undefined_to_string(Description)++"\",
		\"min_value\":\"" ++ undefined_to_string(Min_val)++"\",
		\"max_value\":\"" ++ undefined_to_string(Max_val)++"\",
		\"tags\":\"" ++ undefined_to_string(Tags)++"\",
		\"type\":\"" ++ undefined_to_string(Type)++"\",
		\"accuracy\":\"" ++ undefined_to_string(Accuracy)++"\"
		}",
	{Weight, Result}.


%% @doc
%% Returns an empty string if it was "undefined", else it returns the string itself.
%% @end
-spec undefined_to_string(Text::attr()) -> string().
undefined_to_string(Text) ->
	case Text of
		undefined ->
			"";
		_ ->
			lib_json:to_string(Text)
	end.



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
	io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day,Mstr,Year,Hour,Minute,Second,Micro]).




