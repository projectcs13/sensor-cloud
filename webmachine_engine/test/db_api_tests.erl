%% @author Tholsgård Gabriel
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == db_api_tests ==
%% This module contains several tests to test the functionallity
%% in the module dp_api.
%%
%% @end

-module(db_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("database.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: start_test/0
%% Purpose: Test to start a db_api process
%% Returns: ok | {error, Reason}
%%
%% Side effects: Starts a db_api process
%% @end
-spec start_test() -> ok | {error, term()}.
start_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual(ok, db_api:stop()).


%% @doc
%% Function: add_stream_test/0
%% Purpose: Test to add a stream in the DB.
%% Returns: ok | {error, Reason}
%%
%% Side effects: Creates a row in the 'stream' table.
%% @end
-spec add_stream_test() -> ok | {error, term()}.
add_stream_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej Hej"})),
	?assertEqual(ok, db_api:stop()).


%% @doc
%% Function: clear_stream_table/0
%% Purpose: Test to delete all entires in the 'stream' table.
%% Returns: ok | {error, Reason}
%%
%% Side effects: Creates a row in the 'stream' table. 
%%               Delete all entries in the 'stream' table.
%% @end
-spec clear_stream_table_test() -> ok | {error, term()}.
clear_stream_table_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej"})),
	?assertEqual({atomic, ok}, db_api:clear_stream_table()),
	?assertEqual(ok, db_api:stop()).



%% @doc
%% Function: get_stream_by_id_test/0
%% Purpose: Test to get a stream by id in the DB.
%% Returns: ok | {error, term()}
%%
%% Side effects: Delete all entries in the 'stream' table.
%%               Creates a row in the 'stream' table.
%% @end
-spec get_stream_by_id_test() -> ok | {error, term()}.
get_stream_by_id_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual({atomic, ok}, db_api:clear_stream_table()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej Hej"})),
	?assertMatch(#stream{description="Hej Hej"}, db_api:get_stream_by_id(1)),
	?assertMatch({error, _}, db_api:get_stream_by_id(0)),
	?assertEqual(ok, db_api:stop()).



%% @doc
%% Function: update_stream_test/0
%% Purpose: Test to update a stream by id in the DB.
%% Returns: ok | {error, term()}
%%
%% Side effects: Delete all entries in the 'stream' table.
%%               Creates a row in the 'stream' table.
%%               Updates the created stream with updated data.
%% @end
-spec update_stream_test() -> ok | {error, term()}.
update_stream_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual({atomic, ok}, db_api:clear_stream_table()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej Hej",
												latitude=0.0})),
	?assertMatch(#stream{description="Hej Hej",
						  latitude=0.0}, db_api:get_stream_by_id(1)),
	?assertEqual(ok, db_api:update_stream(#stream{id=1,
												   description="Bye Bye"})),
	?assertMatch(#stream{description="Bye Bye",
						  latitude=0.0}, db_api:get_stream_by_id(1)),
	?assertEqual(ok, db_api:stop()).




%% @doc
%% Function: delete_stream_with_id_test/0
%% Purpose: Test to delete a row via id in the 'stream' table.
%% Returns: ok | {error, term()}
%%
%% Side effects: Delete all entries in the 'stream' table.
%%               Creates a row in the 'stream' table.
%%               Deletes a row in the 'stream' table.
%% @end
-spec delete_stream_with_id_test() -> ok | {error, term()}.
delete_stream_with_id_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual({atomic, ok}, db_api:clear_stream_table()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej Hej"})),
	?assertMatch(#stream{description="Hej Hej"}, db_api:get_stream_by_id(1)),
	?assertEqual(ok, db_api:delete_stream_with_id(1)),
	?assertMatch({error, _}, db_api:get_stream_by_id(1)),
	?assertEqual(ok, db_api:stop()).



%% @doc
%% Function: get_all_streams_test/0
%% Purpose: Test to get all streams in the 'stream' table.
%% Returns: ok | {error, term()}
%%
%% Side effects: Delete all entries in the 'stream' table.
%%               Creates two rows in the 'stream' table.
%% @end
-spec get_all_streams_test() -> ok | {error, term()}.
get_all_streams_test() ->
	?assertEqual({atomic, ok}, db_api:start()),
	?assertEqual({atomic, ok}, db_api:clear_stream_table()),
	?assertEqual(ok, db_api:add_stream(#stream{description="Hej Hej"})),
	?assertEqual(ok, db_api:add_stream(#stream{description="Bye Bye"})),
	?assertEqual(2, length(db_api:get_all_streams())),
	?assertEqual(ok, db_api:stop()).




