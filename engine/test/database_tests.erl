%% @author Tomas Sävström <tosa7943@student.uu.se>
%%   www.csproj13.student.it.uu.se
%% @version 1.0
%% @copyright Copyright information
%% 
%%
%% @doc == database_tests ==
%% Contains test functions for the database module
%%
%% @end
-module(database_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("include/database.hrl").

%% @doc
%% Function: db_test/0
%% Purpose: populate and query the database to check it works
%% Args:   (none)
%% Returns: atom()
%% Side effects: populates and querys a mnesia database at the local node
%% @end

-spec db_test() -> atom().

db_test() ->
% Createing owner, all undefined values will have the value undefined
    Owner = #user{id = 1,
		   email = "tosa7943@student.uu.se"},

% Createing resoruces, all undefined values will have the value undefined
    Resource1 = #resource{id = 1,
			  owner_id = 1},
    Resource2 = #resource{id = 2,
			  owner_id = 1},
    Resource3 = #resource{id = 3,
			  owner_id = 1},

% Creating streams, all undefined values will have the value undefined
    Stream1  = #stream{id= 1,
		       resource_id = 1},
    Stream2  = #stream{id= 2,
		       resource_id = 1},
    Stream3  = #stream{id= 3,
		       resource_id = 1},
    Stream4  = #stream{id= 4,
		       resource_id = 2},
    Stream5  = #stream{id= 5,
		       resource_id = 2},
    Stream6  = #stream{id= 6,
		       resource_id = 2},
    Stream7  = #stream{id= 7,
		       resource_id = 3},
    Stream8  = #stream{id= 8,
		       resource_id = 3},
    Stream9  = #stream{id= 9,
		       resource_id = 3},
    Stream10  = #stream{id= 10,
		       resource_id = 3},
% Inserting owner into the database
    insert_user(Owner),

% Inserting resources into the database
    insert_resource(Resource1),
    insert_resource(Resource2),
    insert_resource(Resource3),
    

% Inserting streams into the database

    insert_stream(Stream1),
    insert_stream(Stream2),
    insert_stream(Stream3),
    insert_stream(Stream4),
    insert_stream(Stream5),
    insert_stream(Stream6),
    insert_stream(Stream7),
    insert_stream(Stream8),
    insert_stream(Stream9),
    insert_stream(Stream10),

    {_,Result1} = find_streams(1),
    {_,Result2} = find_streams(2),
    {_,Result3} = find_streams(3),
    {_,Result4} = find_resources(1),
    {_,Result5} = find_user(1),
    

    ?assert(Result1 == [2,1,3]),
    ?assert(Result2 == [4,5,6]),
    ?assert(Result3 == [8,7,9,10]),
    ?assert(Result4 == [2,1,3]),
    ?assert(Result5 == ["tosa7943@student.uu.se"]).
    
    
    

%% @doc
%% Function: insert_user/1
%% Purpose: insert the user given to the local mnesia database
%% Args:   record()
%% Returns: tuple()
%% Side effects: Writes the given user to the local mnesia database
%% @end

-spec insert_user(User::record()) -> tuple().

insert_user(User) ->    
    Fun = fun() ->
                  mnesia:write(User)
          end,
    mnesia:transaction(Fun).  

%% @doc
%% Function: insert_resource/1
%% Purpose: insert the resource given to the local mnesia database
%% Args:   record()
%% Returns: tuple()
%% Side effects: Writes the given resource to the local mnesia database
%% @end

-spec insert_resource(Resource::record()) -> tuple().

insert_resource(Resource) ->  
    Fun = fun() ->
                  mnesia:write(Resource)
          end,
    mnesia:transaction(Fun).  

%% @doc
%% Function: insert_stream/1
%% Purpose: insert the stream given to the local mnesia database
%% Args:   record()
%% Returns: tuple()
%% Side effects: Writes the given user to the local mnesia database
%% @end

-spec insert_stream(Stream::record()) -> tuple().

insert_stream(Stream) ->
    Fun = fun() ->
                  mnesia:write(Stream)
          end,
    mnesia:transaction(Fun).

%% @doc
%% Function: find_user/1
%% Purpose: returns the email of the user with the given id
%% Args:   int()
%% Returns: tuple()
%% Side effects: Querys the local mnesia database
%% @end

-spec find_user(UserID::integer()) -> tuple().

find_user(UserID) ->
    F = fun() ->
		User = #user{id = UserID, email = '$1'},
		mnesia:select(user, [{User, [], ['$1']}])
        end,
    mnesia:transaction(F).

%% @doc
%% Function: find_resources/1
%% Purpose: returns the resources of the user with the given id
%% Args:   int()
%% Returns: tuple()
%% Side effects: Querys the local mnesia database
%% @end

-spec find_resources(UserID::integer()) -> tuple().

find_resources(UserID) ->
    F = fun() ->
		Resource = #resource{id = '$1',owner_id = UserID, _ = '_'},
		mnesia:select(resource, [{Resource, [], ['$1']}])
        end,
    mnesia:transaction(F).

%% @doc
%% Function: find_streams/1
%% Purpose: returns the streams of the resource with the given id
%% Args:   int()
%% Returns: tuple()
%% Side effects: Querys the local mnesia database
%% @end

-spec find_streams(ResourceID::integer()) -> tuple().


find_streams(ResourceID) ->
    F = fun() ->
		Stream = #stream{id = '$1',resource_id = ResourceID, _ = '_'},
		mnesia:select(stream, [{Stream, [], ['$1']}])
        end,
    mnesia:transaction(F).
