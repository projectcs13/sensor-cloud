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
-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/user.hrl").

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
	erlang:display(parse_path(wrq:path(ReqData))),
	case parse_path(wrq:path(ReqData)) of
		[{"streams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"streams","_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", "_search" ++ _Query}] ->
		  	{['POST', 'GET'], ReqData, State};
		[{"streams"}] ->
			{['POST','GET'], ReqData, State}; 
		[{"users", _UserID}, {"streams"}] ->
			{['GET','POST'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams"}] ->
			{['GET','POST'], ReqData, State};
		[{"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"streams", _StreamID}] ->
			{['GET', 'PUT', 'DELETE'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", _StreamID}] ->
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
%% Purpose: Used to handle DELETE requests by deleteing the stream in elastic search
%% Returns: {Sucess, ReqData, State}, where Sucess is true if delete is sucessful
%%			and false otherwise.
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

delete_resource(ReqData, State) ->
	Id = proplists:get_value('stream', wrq:path_info(ReqData)),
	erlang:display("delete request"),
	erlang:display(Id),
	case erlastic_search:delete_doc(?INDEX,"stream", Id) of
			{error,Reason} -> {false, wrq:set_resp_body(json_encode(Reason),ReqData), State};
			{ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State}
	end.


%% @doc
%% Function: process_post/2
%% Purpose: Used to handle POST requests by creating streams, or search for streams in elastic search
%% Returns: {Sucess, ReqData, State}, where Sucess is true if the post request is 
%%          sucessful and false otherwise.
%% @end
-spec process_post(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_post(ReqData, State) ->
	case is_search(ReqData) of 
		false ->
			erlang:display("Create request"),
			{Stream,_,_} = json_handler(ReqData, State),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserAdded = Stream;
				UserId ->
					UserAdded = add_field(Stream,"owner_id",UserId)
			end,
			case proplists:get_value('res', wrq:path_info(ReqData)) of
				undefined ->
					ResAdded = UserAdded;
				ResId ->
					ResAdded = add_field(UserAdded,"resource_id",ResId)
			end,
			case erlastic_search:index_doc(?INDEX, "stream", ResAdded) of	
				{error, Reason} -> {false, wrq:set_resp_body(json_encode(Reason),ReqData), State};
				{ok,List} -> erlang:display("Sucess"), 
							 {true, wrq:set_resp_body(json_encode(List),ReqData), State}
			end;
		true ->
			process_search(ReqData,State)	
	end.

%% @doc
%% Function: process_search/2
%% Purpose: Used to handle search requests that come from POST or GET requests
%% Returns: {Sucess, ReqData, State}, where Sucess is true if the search request is 
%%          sucessful and false otherwise.
%% @end
-spec process_search(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search(ReqData, State) ->
	erlang:display("search request"),
	URIQuery = wrq:req_qs(ReqData),
	case proplists:get_value('user', wrq:path_info(ReqData)) of
		undefined ->
			UserQuery = [],
			UserDef = false;
		UserId ->
			UserQuery = "owner_id:" ++ UserId,
			UserDef = true
		end,
	case proplists:get_value('res', wrq:path_info(ReqData)) of
		undefined ->
			ResQuery = [],
			ResDef = false;
		ResId ->
			ResQuery = "resource_id:" ++ ResId,
			ResDef = true
	end,
	case ResDef and UserDef of
		true -> Query = UserQuery ++ "&" ++ ResQuery; 
		false -> case ResDef or UserDef of
					 true -> Query = UserQuery ++ ResQuery;
					 false -> Query = ""
				 end
	end,
	FullQuery = lists:append(transform(URIQuery,ResDef or UserDef),Query),
	erlang:display(FullQuery),
	case erlastic_search:search_limit(?INDEX, "stream", FullQuery,10) of % Maybe wanna take more
		{error,Reason} -> {false, wrq:set_resp_body(json_encode(Reason),ReqData), State};
		{ok,List} -> erlang:display(json_encode(List)),
					 {true,wrq:set_resp_body(json_encode(List),ReqData),State} % May need to convert
	end.


%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updateing the given documents in elastic search
%% Returns: {Sucess, ReqData, State}, where Sucess is true if the PUT request is 
%%          sucessful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

put_stream(ReqData, State) ->
	erlang:display("update request"),
	StreamId = proplists:get_value('stream', wrq:path_info(ReqData)),
	{Stream,_,_} = json_handler(ReqData,State),
	Update = create_update(Stream),
	erlang:display(Update),
	case erlastic_search:update_doc(?INDEX, "stream", StreamId, Update) of 
		{error,Reason} -> {false, wrq:set_resp_body(json_encode(Reason),ReqData), State};
		{ok,List} -> {true,wrq:set_resp_body(json_encode(List),ReqData),State}
	end.





%% @doc
%% Function: get_stream/2
%% Purpose: Used to handle GET requests by giveing the document with the given
%%          Id or listing the documents that can be found from the restrictions
%%          given by the URI.
%% Returns: {Sucess, ReqData, State}, where Sucess is true if the PUT request is 
%%          sucessful and false otherwise.
%% @end
-spec get_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

get_stream(ReqData, State) ->
	case is_search(ReqData) of
		true -> process_search(ReqData,State);
		false ->
			erlang:display("fetch request"),
			case proplists:get_value('stream', wrq:path_info(ReqData)) of
				undefined ->
				% List streams based on URI
		    		erlang:display("Value undefined"),
					case proplists:get_value('user', wrq:path_info(ReqData)) of
						undefined ->
							UserQuery = [],
							UserDef = false;
						UserId ->
							UserQuery = "owner_id:" ++ UserId,
							UserDef = true
					end,
					case proplists:get_value('res', wrq:path_info(ReqData)) of
						undefined ->
							ResQuery = [],
							ResDef = false;
						ResId ->
							ResQuery = "resource_id:" ++ ResId,
							ResDef = true
					end,
					case ResDef and UserDef of
						true -> Query = UserQuery ++ "&" ++ ResQuery;
						false -> case ResDef or UserDef of
							 		true -> Query = UserQuery ++ ResQuery;
							 		false -> Query = "*"
								 end
					end,
					erlang:display(Query),
					case erlastic_search:search_limit(?INDEX, "stream", Query, 10) of % Maybe wanna take more
						{error,Reason} -> {{error, Reason}, ReqData, State};
						{ok,List} -> {json_encode(List), ReqData, State} % Maybe need to convert
					end;
				StreamId ->
		        	erlang:display("Value defined"),
				% Get specific stream
					case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
						{error, Msg} -> 
							erlang:display("got error"),
							{Msg, ReqData, State};
						{ok,List} -> 
							erlang:display("got value"),
					     	{json_encode(List), ReqData, State}
					end
				end
	end.

%% @doc
%% Function: is_search/1
%% Purpose: Used to decied if the URI specifys a search
%% Returns: True if URI specifys a search, false otherwise
%% @end
-spec is_search(ReqData::term()) -> boolean().

is_search(ReqData) ->
	URIList = string:tokens(wrq:path(ReqData), "/"),
	IsSearch = (string:sub_string(lists:nth(length(URIList),URIList),1,7) == "_search").

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
%% Function: is_query_empty/1
%% Purpose: Used to decied if the respond from erlastic search found no value
%% Returns: True if URI specifys a search, false otherwise
%% @end
-spec is_query_empty(Respons::term()) -> boolean().

is_query_empty({struct,[{_,_},{_,_},{_,{struct,[{_,_},{_,_},{_,0}]}},_]}) ->
	true;
is_query_empty(_) ->
	false.



create_update(Stream) ->
	"{\n\"doc\" : " ++ Stream ++ "\n}".



add_field(Stream,FieldName,FieldValue) ->
	string:substr(Stream,1,length(Stream)-2) ++ ",\n" ++ FieldName ++ " : " ++ FieldValue ++ "\n}".
	


%% @doc
%% Function: merge_lists/2
%% Purpose: helper function to user_to_json/1, given a list of keys and a list of values, this function
%% will create a list [{Key, Value}], if a value is undefined, it will remove the value and the key
%% that it corresponds, both lists are assumed to be of equal length.
%% Returns: [{Key, Value}] | []
%% @end

%% PRE-COND: Assumes that both lists are of equal size.
merge_lists([], []) -> [];
merge_lists([H|T], [A|B]) ->
	case A of
		undefined -> merge_lists(T,B);
		_ -> [{H,A}]++merge_lists(T,B)
	end.


%% @doc
%% Function: id_from_path/2
%% Purpose: Retrieves the if from the path.
%% Returns: Id
%% @end
id_from_path(RD) ->
    case wrq:path_info(id, RD) of
        undefined->
            ["users", Id] = string:tokens(wrq:disp_path(RD), "/"),
            Id;
        Id -> Id
    end.


parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].

transform([],true) -> "&";
transform([],false) -> "";
transform([{Field,Value}|Rest],AddAnd) ->
	case Rest of 
		[] -> Field ++ ":" ++ Value ++ transform(Rest,AddAnd);
		_ -> Field ++ ":" ++ Value ++ "&" ++ transform(Rest,AddAnd)
	end.

% Taken from erlasticsearch
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).

%% To-do : HTTP Caching support w etags / header expiration.

