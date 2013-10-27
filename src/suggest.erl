%% @author Tommy Mattsson, Georgios Koutsoumpakis
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == suggest ==
%% 
%%  
%%
%% @end
-module(suggest).
-export([init/1, 
	allowed_methods/2, 
	process_post/2, 
	content_types_provided/2, 
	get_suggestion/2, 
	add_suggestion/2,
	update_suggestion/1]).


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
%% Purpose: Used to define what methods are allowed one the given URI's.
%% Returns: {List, ReqData, State}, where list is the allowed methods for the given URI. 
%% @end
-spec allowed_methods(ReqData::term(),State::term()) -> {list(), term(), term()}.

allowed_methods(ReqData, State) ->
	case parse_path(wrq:path(ReqData)) of
		[{"suggest", _Term}] ->
			{['GET'], ReqData, State}; 
		[{"suggest"}] ->
			{['POST'], ReqData, State}; 
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
%% Function: process_post/2
%% Purpose: Used to handle POST requests by creating streams, or search for streams in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the post request is
%% successful and false otherwise.
%% @end
-spec process_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_post(ReqData, State) ->
	{Query,_,_} = json_handler(ReqData, State),	
	case erlastic_search:suggest(?INDEX, Query) of	
		{error, Reason} -> {false, wrq:set_resp_body(json_encode(Reason),ReqData), State};
		{ok,List} -> {true, wrq:set_resp_body(json_encode(List),ReqData), State}
	end.



%% @doc
%% Function: get_suggestion/2
%% Purpose: Used to handle GET requests for suggestions by giving the term 
%% (model)
%% Returns: {String, ReqData, State}
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
				{error, Reason} -> {json_encode(Reason),ReqData, State};
				{ok,List} -> 
					EncodedList = json_encode(List),
					case re:run(EncodedList, "\"options\":\\[\\]", [{capture, first, list}]) of
						{match, _} -> 
							{{halt,404},ReqData, State};
						_->
							{json_encode(List),ReqData, State}
					end
			end
	end.




%% @doc
%% Function: add_suggestion/2aaaaaaaaa
%% Purpose: Used to get the json object from the request
%% Returns: {Json,ReqData,State}
%% @end
add_suggestion(Resource, Json) ->
	ResourceId = binary_to_list(proplists:get_value(<<"_id">>, Json)),
	Manufacturer = lib_json:get_field(Resource, "manufacturer"),
	Model = lib_json:get_field(Resource, "model"),
	Tags = lib_json:get_field(Resource, "tags"),
	Polling_freq = lib_json:get_field(Resource, "polling_freq"),
	Weight = scoring:calc(Resource, resource),
	case Model of 
		undefined ->
			{error, "no model"};
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
				{error, S} -> erlang:display("Suggestion not saved ");
				{ok, _} -> 	ok
			end
	end.


update_suggestion(Stream) ->
	erlang:display("*******Starting Update********"),
	ResourceId = lib_json:get_field(Stream, "resource_id"),
	erlang:display(ResourceId),
	case erlastic_search:search(?INDEX, "suggestion", "resource_id:"++ResourceId) of
		{error, _} -> erlang:display("ERROR");
		{ok, Response} ->
			erlang:display("HEEEREEEEE"),
			erlang:display(lib_json:to_string(Response)),	
			case lib_json:get_field(Response, "hits.hits[0]._source.resource_id") of
				ResourceId ->
					erlang:display("Getting from response"),
					Weight = lib_json:get_field(Response, "hits.hits[0]._source.suggest.weight"),
					Id = lib_json:get_field(Response, "hits.hits[0]._id"),
					erlang:display("suggest id  "++Id),
					Output = lib_json:get_field(Response, "hits.hits[0]._source.suggest.output"),
					Input = lib_json:get_field(Response, "hits.hits[0]._source.suggest.input"),
					Payload = lib_json:get_field(Response, "hits.hits[0]._source.suggest.payload"),
					{AddWeight, StreamInfo} = get_stream_info(Stream),
					NewWeight = Weight + AddWeight,
					Sugg = lib_json:get_field(Response, "hits.hits[0]._source"),
					erlang:display("---------"),
					case lib_json:get_field(Response, "hits.hits[0]._source.suggest.payload.streams") of
						undefined ->
							erlang:display("000000000"),
							NewPayload = lib_json:add_value(Payload, "streams", "["++StreamInfo++"]"),
							erlang:display(NewPayload),
							erlang:display("111111111"),
							%TempSugg = lib_json:replace_field(Sugg, "suggest.payload", lib_json:to_string(NewPayload)),
							%erlang:display(lib_json:to_string(TempSugg)),
							%erlang:display("222222222"),
							%NewSugg = lib_json:replace_field(TempSugg, "suggest.weight", NewWeight),
							%erlang:display(NewSugg);
							erlang:display(ResourceId),
							NewSugg = "{
				\"resource_id\" : \"" ++ ResourceId ++ "\",
				\"suggest\" : {
					\"input\" : [ \"" ++ lib_json:to_string(Input) ++ "\" ], 
					\"output\" : \"" ++ Output ++ "\",
					\"payload\" : " ++ NewPayload ++ ",
					\"weight\" : " ++ integer_to_list(NewWeight) ++ "
				}				
								}",
							erlang:display("FINAL"),
							erlang:display(NewSugg);
						OldStream ->
							erlang:display("333333333"),
							erlang:display("oldstream"),
							erlang:display(lib_json:to_string(OldStream)),

							NewStreamList = lib_json:add_value_in_list(OldStream, StreamInfo),
							erlang:display("444444444"),
							TempSugg = lib_json:replace_field(Sugg, "suggest.payload.streams", NewStreamList),
							erlang:display(lib_json:to_string(TempSugg)),
							erlang:display("555555555"),
							NewSugg = lib_json:replace_field(TempSugg, "suggest.weight", NewWeight)
					end,
					%case erlastic_search:update_doc(?INDEX, "suggestion", Id, lib_json:decode(NewSugg)) of 
					%	{error, {_,S}} -> erlang:display("Suggestion not saved "),
					%		erlang:display(binary_to_list(S));
					%	{ok, _} -> erlang:display("Stream suggestion added")
					%end;
					{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {"http://localhost:9200/sensorcloud/suggestion/"++Id++"/_update", [],"application/json", "{\"doc\":"++NewSugg++"}"}, [], []);
				_ -> 
					erlang:display("error-2")
			end
	end.
post_request(URL, ContentType, Body) -> request(post, {URL, [], ContentType, Body}).

request(Method, Request) ->
	httpc:request(Method, Request, [], []).




get_stream_info(Stream) ->
	Name = lib_json:get_field(Stream, "name"),
	Description = lib_json:get_field(Stream, "description"),
	Min_val  = lib_json:get_field(Stream, "min_val"),
	Max_val  = lib_json:get_field(Stream, "max_val"),
	Tags  = lib_json:get_field(Stream, "tags"),
	Type  = lib_json:get_field(Stream, "Type"),
	Weight = scoring:calc([Name, Description, Min_val, Max_val, Tags, Type]),
	Result ="{
		\"name\":\"" ++ undefined_to_string(Name)++"\",
		\"description\":\"" ++ undefined_to_string(Description)++"\",
		\"min_value\":\"" ++ undefined_to_string(Min_val)++"\",
		\"max_value\":\"" ++ undefined_to_string(Max_val)++"\",
		\"tags\":\"" ++ undefined_to_string(Tags)++"\",
		\"type\":\"" ++ undefined_to_string(Type)++"\"
		}",
	erlang:display(Result),
	{Weight, Result}.




undefined_to_string(Text) ->
	case Text of
		undefined ->
			"";
		_ ->
			Text
	end.


%% @doc
%% Function: json_handler/2
%% Purpose: Used to get the json object from the request
%% Returns: {Json,ReqData,State}
%% @end
-spec json_handler(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	{Value, ReqData, State}.

%% @doc
%% Function: create_update/1
%% Purpose: Used to create the update document sent to erlastic search
%% Returns: The update document to send to erlasticsearch
%% @end
-spec create_update(Stream::string()) -> string().

create_update(Stream) ->
	"{\n\"doc\" : " ++ Stream ++ "\n}".

%% @doc
%% Function: add_field/3
%% Purpose: Used to add a new field to the given string representation of
%%          of a JSON object, the field will be FieldName : FieldValue
%% Returns: The string representation of the JSON object with the new field
%% @end
-spec add_field(Stream::string(),FieldName::string(),FieldValue::term()) -> string().

add_field(Stream,FieldName,FieldValue) ->
	case is_integer(FieldValue) of
		true ->
			string:substr(Stream,1,length(Stream)-1) ++ ",\n\"" ++ FieldName ++ "\" : " ++ FieldValue ++ "\n}";
		false ->
			string:substr(Stream,1,length(Stream)-1) ++ ",\n\"" ++ FieldName ++ "\" : \"" ++ FieldValue ++ "\"\n}"
	end.
			

%% @doc
%% Function: parse_path/1
%% Purpose: Used to parse the URI path
%% Returns: The parsed URI path as a list
%% @end
-spec parse_path(Path::file:name_all()) -> list().

parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

%% @doc
%% Function: pair/1
%% Purpose: Used to create a new list of tuples where each 
%%          2 elements are paired
%% Returns: The paired list
%% @end
-spec pair(PathList::list()) -> list().

pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].



%% @doc
%% Function: json_encode/1
%% Purpose: Used to transform the given data to json
%% Returns: JSON that is created
%% @end

% Taken from erlasticsearch
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).

%% @doc
%% Function: update_doc/4
%% Purpose: Used to update document in elastic search
%% Returns: JSON response from elastic search server
%% @end

% Taken from erlasticsearch and modified to not encode
update_doc(Index, Type, Id, Mochijson) ->
    update_doc(Index, Type, Id, Mochijson, []).

%% @doc
%% Function: update_doc/5
%% Purpose: Used to update document in elastic search
%% Returns: JSON response from elastic search server
%% @end

% Taken from erlasticsearch and modified to not encode
update_doc(Index, Type, Id, Json, Qs) ->
    Id1 = mochiweb_util:quote_plus(Id),
    ReqPath = Index ++ [$/ | Type] ++ [$/ | Id1] ++ "/_update",
    erls_resource:post(#erls_params{}, ReqPath, [], Qs, Json, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Should be moved to own module later
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_timestamp() ->
    TS = {MSec,Sec,Micro} = os:timestamp(),
	{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day,Mstr,Year,Hour,Minute,Second,Micro]).




