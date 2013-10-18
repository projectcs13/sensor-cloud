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
		 delete_resource/2, process_post/2, put_stream/2, get_stream/2]).



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
	case api_help:parse_path(wrq:path(ReqData)) of
		[{"streams", "_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"streams","_search"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams", "_search"}] ->
		  	{['POST', 'GET'], ReqData, State};
		[{"streams"}] ->
			{['POST', 'GET'], ReqData, State}; 
		[{"users", _UserID}, {"streams"}] ->
			{['POST', 'GET'], ReqData, State};
		[{"users", _UserID}, {"resources", _ResourceID}, {"streams"}] ->
			{['POST', 'GET'], ReqData, State};
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
%% Purpose: Used to handle DELETE requests by deleting the stream in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if delete is successful
%% and false otherwise.
%% @end
-spec delete_resource(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

delete_resource(ReqData, State) ->
	Id = proplists:get_value('stream', wrq:path_info(ReqData)),
	erlang:display("delete request"),
	case erlastic_search:delete_doc(?INDEX,"stream", Id) of
			{error,Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason),ReqData), State};
			{ok,List} -> {true,wrq:set_resp_body(api_help:json_encode(List),ReqData),State}
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
			erlang:display("Create request"),
			{Stream,_,_} = api_help:json_handler(ReqData, State),
			case proplists:get_value('user', wrq:path_info(ReqData)) of
				undefined ->
					UserAdded = Stream;
				UserId ->
					UserAdded = api_help:add_field(Stream,"owner_id",UserId)
			end,
			case proplists:get_value('res', wrq:path_info(ReqData)) of
				undefined ->
					ResAdded = UserAdded;
				ResId ->
					ResAdded = api_help:add_field(UserAdded,"resource_id",ResId)
			end,
			case api_help:get_value_field(ResAdded,"resource_id") == [] of
				true -> {false, wrq:set_resp_body("\"resource_id_missing\"",ReqData), State};
				false ->
					case erlastic_search:index_doc(?INDEX, "stream", ResAdded) of	
						{error, Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason),ReqData), State};
						{ok,List} -> {true, wrq:set_resp_body(api_help:json_encode(List),ReqData), State}
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
	FullQuery = lists:append(api_help:transform(URIQuery,ResDef or UserDef),Query),
	case erlastic_search:search_limit(?INDEX, "stream", FullQuery,200) of % Maybe wanna take more
		{error,Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason),ReqData), State};
		{ok,List} -> {true,wrq:set_resp_body(api_help:json_encode(List),ReqData),State} 
	end.


%% @doc
%% Function: process_search_get/2
%% Purpose: Used to handle search requests that come from GET requests
%% Returns: {Success, ReqData, State}, where Success is true if the search request is
%% successful and false otherwise.
%% @end
-spec process_search_get(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

process_search_get(ReqData, State) ->
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
	FullQuery = lists:append(api_help:transform(URIQuery,ResDef or UserDef),Query),
	case erlastic_search:search_limit(?INDEX, "stream", FullQuery,200) of % Maybe wanna take more
		{error,Reason} -> {Reason, ReqData, State};
		{ok,List} -> {api_help:json_encode(List),ReqData,State} 
	end.


%% @doc
%% Function: put_stream/2
%% Purpose: Used to handle PUT requests by updating the given documents in elastic search
%% Returns: {Success, ReqData, State}, where Success is true if the PUT request is
%% successful and false otherwise.
%% @end
-spec put_stream(ReqData::term(),State::term()) -> {boolean(), term(), term()}.

put_stream(ReqData, State) ->
	erlang:display("update request"),
	StreamId = proplists:get_value('stream', wrq:path_info(ReqData)),
	{Stream,_,_} = api_help:json_handler(ReqData,State),
	Update = api_help:create_update(Stream),
	case api_help:update_doc(?INDEX, "stream", StreamId, Update) of 
		{error,Reason} -> {false, wrq:set_resp_body(api_help:json_encode(Reason),ReqData), State};
		{ok,List} -> {true,wrq:set_resp_body(api_help:json_encode(List),ReqData),State}
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
			erlang:display("fetch request"),
			case proplists:get_value('stream', wrq:path_info(ReqData)) of
				undefined ->
				% List streams based on URI
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
						true -> Query = ResQuery;
						false -> case ResDef or UserDef of
							 		true -> Query = UserQuery ++ ResQuery;
							 		false -> Query = "*"
								 end
					end,
					case erlastic_search:search_limit(?INDEX, "stream", Query,200) of % Maybe wanna take more
						{error,Reason} -> {{error, Reason}, ReqData, State};
						{ok,List} -> {api_help:remove_search_part(api_help:make_to_string(api_help:json_encode(List)),false,0), ReqData, State} 
					end;
				StreamId ->
				% Get specific stream
					case erlastic_search:get_doc(?INDEX, "stream", StreamId) of 
						{error, Msg} -> 
							{api_help:json_encode(Msg), ReqData, State};
						{ok,List} -> 
					     	{api_help:json_encode(List), ReqData, State}
					end
				end
	end.

