%% Author: Tomas S�vstr�m <tosa7943@student.uu.se>, Li Hao <hali2222@student.uu.se>
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
-define(ACCEPTEDFIELDSSTREAMS, ["user_id","name","description", "type","tags","private","unit","accuracy","min_val","max_val","polling","uri","polling_freq","location","resource","resource.resource_type","resource.uuid","parser","data_type","location.lon","location.lat"]).

%% Fields for users
-define(RESTRCITEDUPDATEUSERS, ["username"]).
-define(ACCEPTEDFIELDSUSERS, ["username", "email", "firstname", "lastname", "description", "password", "private"]).

%% Fields for resources
-define(RESTRCITEDUPDATERESOURCES, []).
-define(RESTRCITEDCREATERESOURCES, []).
-define(ACCEPTEDFIELDSRESOURCES, ["name","description","model","manufacturer","streams_suggest"]).
%% Fields for data-points
-define(ACCEPTEDFIELDSDATAPOINTS, ["stream_id","timestamp","value"]).
