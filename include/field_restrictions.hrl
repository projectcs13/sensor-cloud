%% Author: Tomas Sävström <tosa7943@student.uu.se>, Li Hao <hali2222@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% == api include file ==
%% Includes defenitions of what fields are accteded and what fields are restricted, 
%% and also what index to use in elastic search
%%
%% @end

%% Index
-define(INDEX, "sensorcloud").

%% Fields for streams
-define(RESTRCITEDUPDATESTREAMS, ["active","quality","user_ranking","subscribers","last_update","creation_date","history_size"]).
-define(RESTRCITEDCREATESTREAMS, ["active","quality","user_ranking","subscribers","last_update","creation_date","history_size"]).
-define(ACCEPTEDFIELDSSTREAMS, ["user_id","name","description", "type","tags","private","unit","accuracy","min_val","max_val","polling","uri","polling_freq","location","resource", "parser", "data_type"]).


%% Fields for users
-define(RESTRCITEDUPDATEUSERS, ["username"]).
-define(ACCEPTEDFIELDSUSERS, ["username","private"]).

%% Fields for resources
-define(RESTRCITEDUPDATERESOURCES, []).
-define(RESTRCITEDCREATERESOURCES, []).
-define(ACCEPTEDFIELDSRESOURCES, ["name","description","model","manufacturer"]).

%% Fields for data-points
-define(ACCEPTEDFIELDSDATAPOINTS, ["stream_id","timestamp","value"]).
