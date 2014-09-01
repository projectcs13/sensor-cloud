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
-define(RESTRICTED_STREAMS_UPDATE, ["active","quality","user_ranking","subscribers","last_update","creation_date"]).
-define(RESTRICTED_STREAMS_CREATE, ["active","quality","user_ranking","subscribers","nr_subscribers","last_update","creation_date"]).
-define(ACCEPTED_STREAMS_FIELDS, ["user_id","name","description", "type","tags","private","unit","accuracy","min_val","max_val","polling","uri","polling_freq","location","resource","resource.resource_type","resource.uuid","parser","data_type","location.lon","location.lat"]).

%% Fields for users
-define(RESTRCITEDUPDATEUSERS, ["username", "subscriptions"]).
-define(ACCEPTEDFIELDSUSERS, ["username", "email", "firstname", "lastname", "description", "password", "private", "access_token", "refresh_token"]).

%% Fields for resources
-define(RESTRICTED_RESOURCES_UPDATE, []).
-define(RESTRICTED_RESOURCES_CREATE, ["streams_suggest"]).
-define(ACCEPTED_RESOURCES_FIELDS, ["name","description","model","manufacturer"]).

%% Fields for data-points
-define(ACCEPTED_DATAPOINTS_FIELDS, ["stream_id","timestamp","value"]).

%% Fields for virtual streams
-define(ACCEPTED_FIELDS_VSTREAMS_UPDATE, ["user_id","name","description","tags","private"]).
-define(ACCEPTED_FIELDS_VSTREAMS_CREATE, ["user_id","name","description","tags","private","function","streams_involved","creation_date","timestampfrom"]).
