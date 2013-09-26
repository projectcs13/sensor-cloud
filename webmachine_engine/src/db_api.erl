%% @author Tholsgård Gabriel
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == db_api ==
%% This module contains all the necessary functionality to connect
%% to a db and query it.
%%
%% @end

-module(db_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, add_stream/1, get_stream_by_id/1, get_all_streams/0,
		 update_stream/2, delete_stream_with_id/1, print_stream/1,
		 clear_stream_table/0]).

-include_lib("database.hrl").

-define(MNESIA_DIR, "/home/kristian/mnesia/").
-define(NODES, [node()]).


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: create_stream_table/0
%% Purpose: Create a table 'stream' in the database for streams.
%% Returns: {atomic, ok} | {aborted, Reason}
%%
%% Side effects: Create 'stream' table in the database following the
%%               specification of the record #stream.
%% @end
-spec create_stream_table() -> {atomic, ok} | {aborted, term()}.
create_stream_table() ->
  case mnesia:create_table(stream,
						 [{attributes, record_info(fields, stream)},
						{disc_copies, ?NODES},
						{type, ordered_set}])
	of
	{atomic, ok} -> {atomic, ok};
	{aborted, {already_exists, _}} -> {atomic, ok};
	{aborted, Reason} -> {aborted, Reason}
end.



%% @doc
%% Function: start/0
%% Purpose: Start a node for database connection.
%% Returns: ok | {error, Reason}
%%
%% Side effects: Starts a node for handling the database connection.
%%               Creates a database on disc.
%% @end
-spec start() -> ok | {error, term()}.
start() ->
	application:set_env(mnesia, dir, ?MNESIA_DIR),
	mnesia:create_schema(?NODES),
	application:start(mnesia),
	create_stream_table().



%% @doc
%% Function: stop/0
%% Purpose: Stops the node for the database connection.
%% Returns: ok | {error, Reason}
%%
%% @end
-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(mnesia).




%% @doc
%% Function: add_stream/1
%% Purpose: Insert a stream in the database.
%% Returns: {aborted, Reason} | ok | exit(Reason)
%%
%% Side effects: Inserts a row in the table 'stream' in the database
%%               with the specified data.
%%               Id of the stream is set to the highest id in the table + 1.
%% @end
-spec add_stream(record) -> {aborted, term()} | ok | {error, term()}.
add_stream(Stream_record) when is_record(Stream_record, stream) ->
	Trans = fun() ->
					case mnesia:last(stream) of
						'$end_of_table' -> Id = 1;
						I -> Id = I + 1
					end,
					mnesia:write(Stream_record#stream{id=Id})
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).



%% @doc
%% Function: get_all_streams/0
%% Purpose: Read all streams from the database.
%% Returns: {aborted, Reason} | {error, Reason} | [Record] (#stream)
%%
%% @end
-spec get_all_streams() -> {aborted, term()} | {error, term()} | [record].
get_all_streams() ->
	Trans = fun() ->
					case mnesia:match_object(#stream{_='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "No Such ID"};
						S -> S
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).




%% @doc
%% Function: get_stream_by_id/1
%% Purpose: Read a stream from the database.
%% Returns: {aborted, Reason} | {error, Reason} | Record (#stream)
%%
%% @end
-spec get_stream_by_id(integer()) -> {aborted, term()} |
									   {error, term()} | 
									   record.
get_stream_by_id(Id) when is_integer(Id) ->
	Trans = fun() ->
					case mnesia:match_object(#stream{id=Id, _='_'}) of
						{aborted, Reason} -> {aborted, Reason};
						[] -> {error, "No Such ID"};
						[S|_] -> S
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).




%% @doc
%% Function: update_stream/1
%% Purpose: Update a stream in the database.
%% Returns: {aborted, Reason} | {error, Reason} | ok
%%
%% Side effects: Updates values of an existing stream, with the same id
%%               as the id in the provided stream, in the database that
%%               are not undefined in the provided stream.
%% @end
-spec update_stream(integer(), record()) -> {aborted, term()} | {error, term()} | ok.
update_stream(Id, Stream) when is_record(Stream, stream) ->
	case get_stream_by_id(Id) of
		{aborted, Reason} -> {aborted, Reason};
		{error, Reason} -> {error, Reason};
		Record ->
			Updated_record =
				Record#stream{
							   type =
								   case Stream#stream.type of
									   undefined -> Record#stream.type;
									   Type -> Type
								   end,
							   latitude =
								   case Stream#stream.latitude of
									   undefined ->
										   Record#stream.latitude;
									   Lat -> Lat
								   end,
							   longitude =
								   case Stream#stream.longitude of
									   undefined ->
										   Record#stream.longitude;
									   Long -> Long
								   end,
							   description =
								   case Stream#stream.description of
									   undefined ->
										   Record#stream.description;
									   Desc -> Desc
								   end,
							   public_access =
								   case Stream#stream.public_access of
									   undefined ->
										   Record#stream.public_access;
									   P_A -> P_A
								   end,
							   public_search =
								   case Stream#stream.public_search of
									   undefined ->
										   Record#stream.public_search;
									   P_S -> P_S
								   end,
							   frozen =
								   case Stream#stream.frozen of
									   undefined -> Record#stream.frozen;
									   Frozen -> Frozen
								   end,
							   history_size =
								   case Stream#stream.history_size of
									   undefined ->
										   Record#stream.history_size;
									   H_S -> H_S
								   end,
							   last_updated =
								   case Stream#stream.last_updated of
									   undefined ->
										   Record#stream.last_updated;
									   L_U -> L_U
								   end,
							   secret_key =
								   case Stream#stream.secret_key of
									   undefined -> Record#stream.secret_key;
									   S_K -> S_K
								   end,
							   owner_id =
								   case Stream#stream.owner_id of
									   undefined -> Record#stream.owner_id;
									   Owner_ID -> Owner_ID
								   end,
							   resource_id =
								   case Stream#stream.resource_id of
									   undefined -> Record#stream.resource_id;
									   Res_ID -> Res_ID
								   end,
							   version =
								   case Stream#stream.version of
									   undefined -> Record#stream.version;
									   Version -> Version
								   end
							  },
			Trans = fun() ->
							mnesia:write(Updated_record)
					end,
			mnesia:wait_for_tables([stream], 5000),
			mnesia:activity(transaction, Trans)
	end.



%% @doc
%% Function: delete_stream_with_id/1
%% Purpose: Delete a stream from the database.
%% Returns: ok | {error, Reason} | {aborted, Reason}
%%
%% Side effect: Deletes a row from the database in the table 'stream'
%%              that have id equal to the provided id.
%% @end
-spec delete_stream_with_id(integer()) -> 
		  ok | {error, term()} | {aborted, term()}.
delete_stream_with_id(Id) when is_integer(Id) ->
	Trans = fun() ->
					case mnesia:delete({stream, Id}) of
						{aborted, Reason} -> {aborted, Reason};
						ok -> ok
					end
			end,
	mnesia:wait_for_tables([stream], 5000),
	mnesia:activity(transaction, Trans).
										 



%% @doc
%% Function: print_stream/0
%% Purpose: Print a stream from the database to stdout.
%% Returns: ok
%%
%% Side effect: Writes to stdout
%% @end
-spec print_stream(record) -> ok.
print_stream(#stream{id=Id, type=Type, latitude=Lat, longitude=Long,
					 description=Desc, public_access=P_A,
					 public_search=P_S, frozen=F, history_size=H_S,
					 last_updated=L_U, secret_key=S_K, owner_id=O_Id,
					 resource_id=R_Id, version=Ver}) ->
	io:format("The stream:~nID: ~w~nType: ~w~nLat: ~w~nLong: ~w~n"
			 ++ "Desc: ~s~nPublic_access: ~w~nPublic_search: ~w~n"
			 ++ "Frozen: ~w~nHist_size: ~w~nLast_up: ~w~n"
			 ++ "Secret_key: ~w~nOwner_id: ~w~nResource_id: ~w~n"
			 ++ "Version: ~w~n", [Id, Type, Lat, Long, Desc, P_A, P_S, F, H_S,
								  L_U, S_K, O_Id, R_Id, Ver]).





%% @doc
%% Function: clear_stream_table/0
%% Purpose: Clear all entries in the 'stream' table.
%% Returns: {atomic, ok} | {aborted, Reason}
%%
%% Side effect: Clears all entries in the 'stream' database table.
%% @end
-spec clear_stream_table() -> {atomic,ok} | {aborted, term()}.
clear_stream_table() ->
	mnesia:wait_for_tables([stream], 5000),
	mnesia:clear_table(stream).


