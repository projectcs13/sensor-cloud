
-module(groups).
-export([init/1,
		 allowed_methods/2,
		 exists/3,
		 content_types_accepted/2,
		 put_handler/2,
		 put_group/2]).

-include_lib("webmachine.hrl").
-include_lib("erlastic_search.hrl").


%% Index and Type definitions

-define(INDEX, "cloud").
-define(GROUP, "group").
-define(USER,  "user").
-define(STREAM, "stream").

%% @doc
%% Function: init/1
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end
-spec init([]) -> {ok, undefined}.
init([]) -> 
	 %% start this in the make file somehow
    {ok, undefined}.

%% @doc
%% Function: allowed_methods/2
%% Purpose: init function used to fetch path information from webmachine dispatcher.
%% Returns: {ok, undefined}
%% @end

allowed_methods(ReqData, State) ->
	erlang:display(parse_path(wrq:path(ReqData))),
	case parse_path(wrq:path(ReqData)) of
		[{"users", _UserID}, {"groups", "_search"}] ->
		  	{['GET'], ReqData, State};
		[{"users", _UserID}, {"groups", _GroupID}] ->
			{['POST','GET','PUT','DELETE'], ReqData, State}; 
		[{"users", _UserID}, {"groups"}] ->
			{['GET'], ReqData, State};
		[{"groups", _GroupID}] ->
			{['GET','PUT'], ReqData, State};
		[{"groups"}] ->
			{['GET'], ReqData, State};
		[error] ->
		    {['POST', 'GET'], ReqData, State} % Probably should give som error message
end.

%% @doc
%% Function exists/3
%% Purpose: Given a index, type and Id corresponding to elasticsearch, 
%%          this function checks the existence of a document.
%% Returns: true | false
%% @end

exists(Index, Type, Id) ->
	case erlastic_search:get_doc(Index, Type, Id) of
		{ok, _Data} -> true;
		{error, _Error} -> false
	end.

%% @doc
%% Function: content_types_accepted/2
%% Purpose: based on the content-type on a 'POST' or 'PUT', we know which kind of data that is 
%% allowed to be sent to the server.
%% A code 406 is returned to the client if we don't accept a media type that the client has sent.
%% Returns: {[{Mediatype, Handler}], ReqData, State}
%% @end
content_types_accepted(ReqData, State) ->
	{[{"application/json", put_handler}], ReqData, State}.

%% @doc
%% Function: put_handler/2
%% Purpose: Checks to see if the user id and/or group id exists in elasticsearch
%%			 before updating the group document.
%% Returns : {true, ReqData, State} | {{error, Reason}, ReqData, State} | {error, ReqData, State}


put_handler(ReqData, State) ->
	case parse_path(wrq:path(ReqData)) of
		[{"users", UserID}, {"groups", "_search"}] ->
			case exists(?INDEX, ?USER, UserID) of
				true -> put_group(ReqData, State);
				false -> {{error, "User ID does not exist!"}, ReqData, State}
			end;
		[{"users", UserID}, {"groups", GroupID}] ->
			User_cond = exists(?INDEX, ?USER, UserID),
			Group_cond = exists(?INDEX, ?GROUP, GroupID),
			erlang:display("User : "++User_cond),
			erlang:display("Group : "++Group_cond),
			case (User_cond =:= true) and (Group_cond =:= true) of
				true -> put_group(ReqData, State),
						{true, ReqData, State};
				false -> {{error, "User / Group ID does not exist!"}, ReqData, State}
			end;
		[{"users", UserID}, {"groups"}] ->
			case exists(?INDEX, ?USER, UserID) of
				true -> put_group(ReqData, State),
						{true, ReqData, State};
				false -> {{error, "User ID does not exist!"}, ReqData, State}
			end;
		[{"groups", GroupID}] ->
			case exists(?INDEX, ?GROUP, GroupID) of
				true -> put_group(ReqData, State),
						{true, ReqData, State};
				false -> {{error, "Group ID does not exist!"}, ReqData, State}
			end;
		[error] -> {error, ReqData, State}
	end.

%% @doc
%% Function: put_group/2
%% Purpose: updates / replaces fields values in a group document 
%%			with the data contained in the request body.
%% Side-Effect : The new data will replace the existing data.

put_group(ReqData, State) ->
	GroupId = proplists:get_value(group, wrq:path_info(ReqData)),
	{ReqBody,_,_} = json_handler(ReqData,State),
	Update = create_update(ReqBody),
	case update_doc(?INDEX, ?GROUP, GroupId, Update) of 
		{error,Reason} -> {{error,Reason}, ReqData, State};
		{ok,List} -> {List,ReqData,State}
	end.

%%% @doc
%% Function: json_handler/2
%% Purpose : Returns the request body of a request.
%% Returns : {Value, ReqData, State}

json_handler(ReqData, State) ->
	[{Value,_ }] = mochiweb_util:parse_qs(wrq:req_body(ReqData)), 
	%%{struct, JsonData} = mochijson2:decode(Value),
	{Value, ReqData, State}.

%% @doc
%% Function : create_update/1
%% Purpose : creates a string that is used to update a field in a document.
%% Returns String :: list()
create_update(Stream) ->
	"{\n\"doc\" : " ++ Stream ++ "\n}".

%% @doc
%% Function : parse_path/1
%% Purpose : Pairs elements in a list 'Path' so that the first element pairs with the second.
%% Returns : [] | [{Element}] | [{A1, A2}...{An-1, An}]
parse_path(Path) -> 
	[_|T] = filename:split(Path),
	pair(T).

%% @doc
%% Function : pair/2
%% Purpose : Helper function for parse path, 
%% 			 Pairs elements in a list 'Path' so that the first element pairs with the second.
%% Returns : [] | [{Element}] | [{A1, A2}...{An-1, An}]
pair([]) -> [];
pair([A]) -> [{A}];
pair([A,B|T]) ->
	[{A,B}|pair(T)].

update_doc(Index, Type, Id, Mochijson) ->
    update_doc(Index, Type, Id, Mochijson, []).
update_doc(Index, Type, Id, Mochijson, Qs) ->
    Id1 = mochiweb_util:quote_plus(Id),
    ReqPath = Index ++ [$/ | Type] ++ [$/ | Id1] ++ "/_update",
    erls_resource:post(#erls_params{}, ReqPath, [], Qs, Mochijson, []).

