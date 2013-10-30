%% @author Tomas Sävström <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == dummy ==
%% This module is used to add some dummy data to the system
%% using the RESTful API of webmachine
%%
%% @end
-module(dummy).

-export([add_data/0]).

%% @doc
%% Function: sinus_random_data/3
%% Purpose: Used to add a given amount of datapoints with values
%%          following the sinus curve with some random noise
%% Returns: ok 
%%
%% Side effects: creates datapoints in elastich search
%% @end
-spec sinus_random_data(Amount::integer(),StreamId::string(),Delay::integer()) -> ok.

sinus_random_data(0,_StreamId,_Delay) ->
	ok;
sinus_random_data(Amount,StreamId,Delay) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	{{Year,Month,Day},{Hour,Minute,Second}} = calendar:local_time(),
	TimeStamp = generate_timestamp([Year,Month,Day,Hour,Minute,Second],0),
	Value = math:sin(Amount/10.0) + (random:uniform()/10.0),
	UsedValue = float_to_list(Value,[{decimals,3}]),
	%% Unsure if this is how the datapoints should look
	JSON = "{\"stream_id\":\""++ StreamId++"\",\"timestamp\":\"" ++ TimeStamp ++ "\",\"value\":"++ UsedValue ++"}",
	{ok, {{_Version, _Code, _ReasonPhrase}, _Headers, _Body}} = httpc:request(post, {"http://localhost:8000/streams/" ++ StreamId ++ "/data", [],"application/json", JSON}, [], []),
	timer:sleep(Delay),
	sinus_random_data(Amount-1,StreamId,Delay).


%% @doc
%% Function: random_data/2
%% Purpose: Used to create a timestamp valid in ES
%%          from the input which should be the list
%%          [Year,Mount,Day,Hour,Minute,Day]
%% Returns: The generated timestamp
%%
%% @end
-spec generate_timestamp(DateList::list(),Count::integer()) -> string().

generate_timestamp([],_) ->
	[];
generate_timestamp([First|Rest],3) ->
	case First < 10 of
		true -> "T0" ++ integer_to_list(First) ++ generate_timestamp(Rest,4);
		false -> "T" ++ integer_to_list(First) ++ generate_timestamp(Rest,4)
	end;
generate_timestamp([First|Rest],Count) ->
	case First < 10 of
		true -> "0" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1);
		false -> "" ++ integer_to_list(First) ++ generate_timestamp(Rest,Count+1)
	end.

%% @doc
%% Function: add_data/0
%% Purpose: Used to add some dummy users, resources, streams 
%%          and datapoints to elasticsearch
%% Returns: ok 
%%
%% Side effects: creates users,resources,streams and datapoints in elastich search
%% @end
-spec add_data() -> ok.

add_data() ->
	inets:start(),
	%% Create Users, assumes Webmachine is running on same computer
	{ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, Body1}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\":\"Tomas\"}"}, [], []),
	{ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\":\"Alberto\"}"}, [], []),
	{ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\":\"Quentin\"}"}, [], []),
	{ok, {{_Version4, 200, _ReasonPhrase4}, _Headers4, Body4}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\":\"Andreas\"}"}, [], []),
	{ok, {{_Version5, 200, _ReasonPhrase5}, _Headers5, Body5}} = httpc:request(post, {"http://localhost:8000/users", [],"application/json", "{\"username\":\"Jacob\"}"}, [], []),
	User1 = lib_json:get_field(Body1,"_id"),
	User2 = lib_json:get_field(Body2,"_id"),
	User3 = lib_json:get_field(Body3,"_id"),
	User4 = lib_json:get_field(Body4,"_id"),
	_User5 = lib_json:get_field(Body5,"_id"),
	
	%% Create resources for user1
	{ok, {{_Version6, 200, _ReasonPhrase6}, _Headers6, Body6}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User1 ++"\",\"name\":\"Resource1\",\"tags\":\"[temperature,Uppsala]\",\"description\":\"Temperature in Uppsala\",\"type\":\"Temperature\",\"manufacturer\":\"Ericsson\",\"model\":\"MES013\",\"make\":\"\",\"serial\":\"0943123\",\"location\":\"59.8581,17.6447\",\"active\":\"true\",\"uri\":\"http://uppsala.com\",\"polling_freq\":\"20\",\"creation_date\":\"2013-09-23\"}"}, [], []),
	{ok, {{_Version7, 200, _ReasonPhrase7}, _Headers7, Body7}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User1 ++"\",\"name\":\"Resource2\",\"tags\":\"[temperature,Norrtalje]\",\"description\":\"Temperature in Norrtalje\",\"type\":\"Temperature\",\"manufacturer\":\"Ericsson\",\"model\":\"MES013\",\"make\":\"\",\"serial\":\"0943124\",\"location\":\"59.7667,18.7000\",\"active\":\"true\",\"uri\":\"http://norrtalje.com\",\"polling_freq\":\"20\",\"creation_date\":\"2013-09-23\"}"}, [], []),
	{ok, {{_Version8, 200, _ReasonPhrase8}, _Headers8, Body8}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User1 ++"\",\"name\":\"Resource3\",\"tags\":\"[temperature,Sala]\",\"description\":\"Temperature in Sala\",\"type\":\"Temperature\",\"manufacturer\":\"Ericsson\",\"model\":\"MES013\",\"make\":\"\",\"serial\":\"0943125\",\"location\":\"59.9167,16.6000\",\"active\":\"true\",\"uri\":\"http://sala.com\",\"polling_freq\":\"20\",\"creation_date\":\"2013-09-23\"}"}, [], []),
	U1Resource1 = lib_json:get_field(Body6,"_id"),
	U1Resource2 = lib_json:get_field(Body7,"_id"),
	U1Resource3 = lib_json:get_field(Body8,"_id"),
	
	%% Create streams for resource1 for user1
	{ok, {{_Version9, 200, _ReasonPhrase9}, _Headers9, Body9}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource1 ++"\",\"name\":\"Stream1\",\"tags\":\"[temperature,celsius,Uppsala]\",\"description\":\"Temperature in Celsius in Uppsala\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"celsius\",\"accuracy\":0.95,\"min_val\":-50.0,\"max_val\":50.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":60,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	{ok, {{_Version10, 200, _ReasonPhrase10}, _Headers10, Body10}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource1 ++"\",\"name\":\"Stream2\",\"tags\":\"[temperature,kelvin,Uppsala]\",\"description\":\"Temperature in Kelvin in Uppsala\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"kelvin\",\"accuracy\":0.95,\"min_val\":150.0,\"max_val\":350.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":655,\"last_updated\":\"2013-10-07T010909.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 10000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	U1R1Stream1 = lib_json:get_field(Body9,"_id"),
	U1R1Stream2 = lib_json:get_field(Body10,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U1R1Stream1,1000),
	sinus_random_data(15,U1R1Stream2,1000),
	
	%% Create streams for resource2 for user1
	{ok, {{_Version11, 200, _ReasonPhrase11}, _Headers11, Body11}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource2 ++"\",\"name\":\"Stream1\",\"tags\":\"[temperature,celsius,Norrtalje]\",\"description\":\"Temperature in Celsius in Norrtalje\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"celsius\",\"accuracy\":0.95,\"min_val\":-50.0,\"max_val\":50.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":6,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.7667,18.7000\"}"}, [], []),
	{ok, {{_Version12, 200, _ReasonPhrase12}, _Headers12, Body12}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource2 ++"\",\"name\":\"Stream2\",\"tags\":\"[temperature,kelvin,Norrtalje]\",\"description\":\"Temperature in Kelvin in Norrtalje\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"kelvin\",\"accuracy\":0.95,\"min_val\":150.0,\"max_val\":350.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":55,\"last_updated\":\"2013-10-07T010909.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 10000,\"location\":\"59.7667,18.7000\"}"}, [], []),
	U1R2Stream1 = lib_json:get_field(Body11,"_id"),
	U1R2Stream2 = lib_json:get_field(Body12,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U1R2Stream1,1000),
	sinus_random_data(15,U1R2Stream2,1000),
	
	%% Create streams for resource3 for user1
	{ok, {{_Version13, 200, _ReasonPhrase13}, _Headers13, Body13}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource3 ++"\",\"name\":\"Stream1\",\"tags\":\"[temperature,celsius,Sala]\",\"description\":\"Temperature in Celsius in Sala\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"celsius\",\"accuracy\":0.95,\"min_val\":-50.0,\"max_val\":50.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":16,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.9167,16.6000\"}"}, [], []),
	{ok, {{_Version14, 200, _ReasonPhrase14}, _Headers14, Body14}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U1Resource3 ++"\",\"name\":\"Stream2\",\"tags\":\"[temperature,kelvin,Sala]\",\"description\":\"Temperature in Kelvin in Sala\",\"private\":\"false\",\"type\":\"Temperature\",\"unit\":\"kelvin\",\"accuracy\":0.95,\"min_val\":150.0,\"max_val\":350.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":155,\"last_updated\":\"2013-10-07T010909.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 10000,\"location\":\"59.9167,16.6000\"}"}, [], []),
	U1R3Stream1 = lib_json:get_field(Body13,"_id"),
	U1R3Stream2 = lib_json:get_field(Body14,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U1R3Stream1,1000),
	sinus_random_data(15,U1R3Stream2,1000),
	
	
	
	%% Create resources for user2
	{ok, {{_Version15, 200, _ReasonPhrase15}, _Headers15, Body15}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User2 ++"\",\"name\":\"Resource1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Uppsala\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLS044\",\"make\":\"\",\"serial\":\"09442353\",\"location\":\"59.8581,17.6447\",\"active\":\"true\",\"uri\":\"http://uppsala.com\",\"polling_freq\":\"20\",\"creation_date\":\"2013-09-25\"}"}, [], []),
	{ok, {{_Version16, 200, _ReasonPhrase16}, _Headers16, Body16}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User2 ++"\",\"name\":\"Resource2\",\"tags\":\"[water-level,Norrtalje]\",\"description\":\"Water-level in Norrtalje\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLS044\",\"make\":\"\",\"serial\":\"09442353\",\"location\":\"59.7667,18.7000\",\"active\":\"true\",\"uri\":\"http://norrtalje.com\",\"polling_freq\":\"20\",\"creation_date\":\"2013-09-25\"}"}, [], []),
	U2Resource1 = lib_json:get_field(Body15,"_id"),
	U2Resource2 = lib_json:get_field(Body16,"_id"),

	
	
	%% Create streams for resource1 for user2
	{ok, {{_Version17, 200, _ReasonPhrase17}, _Headers17, Body17}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U2Resource1 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"false\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":15.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":169,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	{ok, {{_Version18, 200, _ReasonPhrase18}, _Headers18, Body18}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U2Resource1 ++"\",\"name\":\"Stream2\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"true\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":15.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":169,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	{ok, {{_Version19, 200, _ReasonPhrase19}, _Headers19, Body19}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U2Resource1 ++"\",\"name\":\"Stream3\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"true\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":15.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":169,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	U2R1Stream1 = lib_json:get_field(Body17,"_id"),
	U2R1Stream2 = lib_json:get_field(Body18,"_id"),
	U2R1Stream3 = lib_json:get_field(Body19,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U2R1Stream1,1000),
	sinus_random_data(15,U2R1Stream2,1000),
	sinus_random_data(15,U2R1Stream3,1000),
		
	%% Create stream for resource2 for user2
	{ok, {{_Version20, 200, _ReasonPhrase20}, _Headers20, Body20}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U2Resource2 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"true\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":15.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":169,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	U2R2Stream1 = lib_json:get_field(Body20,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U2R2Stream1,1000),

	

	%% Create resource for user3
	{ok, {{_Version21, 200, _ReasonPhrase21}, _Headers21, Body21}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User3 ++"\",\"name\":\"Resource1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Uppsala\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLS044\",\"make\":\"\",\"serial\":\"09442358\",\"location\":\"59.8581,17.6447\",\"active\":\"true\",\"uri\":\"http://uppsala.com\",\"polling_freq\":\"40\",\"creation_date\":\"2013-09-29\"}"}, [], []),
	U3Resource1 = lib_json:get_field(Body21,"_id"),
	
	%% Create stream for resource1 for user3
	{ok, {{_Version22, 200, _ReasonPhrase22}, _Headers22, Body22}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U3Resource1 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"true\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":15.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":169,\"last_updated\":\"2013-10-05T010502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 5000,\"location\":\"59.8581,17.6447\"}"}, [], []),
	U3R1Stream1 = lib_json:get_field(Body22,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U3R1Stream1,1000),

	%% Create resources for user4
	{ok, {{_Version23, 200, _ReasonPhrase23}, _Headers23, Body23}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User4 ++"\",\"name\":\"Resource1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Uppsala\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLA099\",\"make\":\"\",\"serial\":\"AACV223\",\"location\":\"59.8581,17.6447\",\"active\":\"true\",\"uri\":\"http://uppsala.com\",\"polling_freq\":\"60\",\"creation_date\":\"2013-10-05\"}"}, [], []),
	{ok, {{_Version24, 200, _ReasonPhrase24}, _Headers24, Body24}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User4 ++"\",\"name\":\"Resource2\",\"tags\":\"[water-level,Norrtalje]\",\"description\":\"Water-level in Norrtalje\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLA099\",\"make\":\"\",\"serial\":\"AACV224\",\"location\":\"59.7667,18.7000\",\"active\":\"true\",\"uri\":\"http://norrtalje.com\",\"polling_freq\":\"60\",\"creation_date\":\"2013-10-05\"}"}, [], []),
	{ok, {{_Version25, 200, _ReasonPhrase25}, _Headers25, Body25}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User4 ++"\",\"name\":\"Resource3\",\"tags\":\"[water-level,Sala]\",\"description\":\"Water-level in Sala\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLA099\",\"make\":\"\",\"serial\":\"AACV225\",\"location\":\"59.9167,16.6000\",\"active\":\"true\",\"uri\":\"http://sala.com\",\"polling_freq\":\"60\",\"creation_date\":\"2013-10-05\"}"}, [], []),
	{ok, {{_Version26, 200, _ReasonPhrase26}, _Headers26, Body26}} = httpc:request(post, {"http://localhost:8000/resources", [],"application/json", "{\"user_id\":\""++ User4 ++"\",\"name\":\"Resource4\",\"tags\":\"[water-level,Stockholm]\",\"description\":\"Water-level in Stockholm\",\"type\":\"Water-level\",\"manufacturer\":\"Ericsson\",\"model\":\"WLA099\",\"make\":\"\",\"serial\":\"AACV227\",\"location\":\"59.3294,18.0686\",\"active\":\"true\",\"uri\":\"http://stockholm.com\",\"polling_freq\":\"60\",\"creation_date\":\"2013-10-05\"}"}, [], []),
	U4Resource1 = lib_json:get_field(Body23,"_id"),
	U4Resource2 = lib_json:get_field(Body24,"_id"),
	U4Resource3 = lib_json:get_field(Body25,"_id"),
	U4Resource4 = lib_json:get_field(Body26,"_id"),
	
	%% Create stream for resource1 for user4
	{ok, {{_Version27, 200, _ReasonPhrase27}, _Headers27, Body27}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U4Resource1 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Uppsala]\",\"description\":\"Water-level in Fyrisan in Uppsala\",\"private\":\"false\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.85,\"min_val\":0.0,\"max_val\":19.0,\"active\":\"true\",\"user_ranking\":10.0,\"subscribers\":1,\"last_updated\":\"2013-10-05T020502.0000\", \"creation_date\":\"2013-10-02\",\"history_size\": 50,\"location\":\"59.8581,17.6447\"}"}, [], []),
	U4R1Stream1 = lib_json:get_field(Body27,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U4R1Stream1,1000),
	
	%% Create stream for resource2 for user4
	{ok, {{_Version28, 200, _ReasonPhrase28}, _Headers28, Body28}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U4Resource2 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Norrtalje]\",\"description\":\"Water-level in Nortaljean in Uppsala\",\"private\":\"false\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.95,\"min_val\":0.0,\"max_val\":10.0,\"active\":\"true\",\"user_ranking\":17.0,\"subscribers\":19,\"last_updated\":\"2013-10-08T070502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 700,\"location\":\"59.7667,18.7000\"}"}, [], []),
	U4R2Stream1 = lib_json:get_field(Body28,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U4R2Stream1,1000),

	%% Create stream for resource3 for user4
	{ok, {{_Version29, 200, _ReasonPhrase29}, _Headers29, Body29}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U4Resource3 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Sala]\",\"description\":\"Water-level in the spring in Sala\",\"private\":\"false\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.75,\"min_val\":0.0,\"max_val\":9.0,\"active\":\"true\",\"user_ranking\":5.0,\"subscribers\":9,\"last_updated\":\"2013-10-09T060502.0000\", \"creation_date\":\"2013-10-01\",\"history_size\": 8900,\"location\":\"59.9167,16.6000\"}"}, [], []),
	U4R3Stream1 = lib_json:get_field(Body29,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U4R3Stream1,1000),

	%% Create stream for resource4 for user4
	{ok, {{_Version30, 200, _ReasonPhrase30}, _Headers30, Body30}} = httpc:request(post, {"http://localhost:8000/streams", [],"application/json", "{\"resource_id\":\""++ U4Resource4 ++"\",\"name\":\"Stream1\",\"tags\":\"[water-level,Stockholm]\",\"description\":\"Water-level in the spring in Stockholm\",\"private\":\"false\",\"type\":\"Water-level\",\"unit\":\"meter\",\"accuracy\":0.97,\"min_val\":0.0,\"max_val\":22.0,\"active\":\"true\",\"user_ranking\":18.0,\"subscribers\":1798,\"last_updated\":\"2013-10-15T010502.0000\", \"creation_date\":\"2013-09-01\",\"history_size\": 50000,\"location\":\"59.3294,18.0686\"}"}, [], []),
	U4R4Stream1 = lib_json:get_field(Body30,"_id"),
	
	%% Add random data between min and max for the streams
	sinus_random_data(15,U4R4Stream1,1000),
	ok.

	
	


	