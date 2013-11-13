%% Author: Tomas Sävström <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% == api include file ==
%% Includes defenitions of what fields are accteded and what fields are restricted, 
%% and also what index to use in elastic search
%%
%% @end

%% Index
-define(INDEX, "sensorcloud").

%% Fields for streams
-define(RESTRCITEDUPDATESTREAMS, ["quality","user_ranking","subscribers","last_update","creation_date","history_size"]).
-define(RESTRCITEDCREATESTREAMS, ["quality","user_ranking","subscribers","last_update","creation_date","history_size"]).
-define(ACCEPTEDFIELDSSTREAMS, ["resource_id","name","tags","description","private","type","accuracy","min_val","max_val","quality","active","user_ranking","subscribers","last_updated","creation_date","history_size","location"]).


%% Fields for users
-define(ACCEPTEDFIELDSUSERS, ["username","private"]).

%% Fields for resources
-define(RESTRCITEDUPDATERESOURCES, ["creation_date"]).
-define(RESTRCITEDCREATERESOURCES, ["creation_date"]).
-define(ACCEPTEDFIELDSRESOURCES, ["user_id","name","tags","description","type","manufacturer","uri","polling_freq","creation_date","uuid"]).

%% Fields for data-points
-define(ACCEPTEDFIELDSDATAPOINTS, ["streamid","timestamp","value"]).