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
-define(ACCEPTEDFIELDSSTREAMS, ["resource","name","tags","description","private","type","accuracy","min_val",
								"max_val","quality","active","user_ranking","subscribers","last_update","creation_date",
								"history_size","location","user_id","uri","polling_freq","uuid","unit"]).


%% Fields for users
-define(ACCEPTEDFIELDSUSERS, ["username","private"]).

%% Fields for resources
-define(RESTRCITEDUPDATERESOURCES, []).
-define(RESTRCITEDCREATERESOURCES, []).
-define(ACCEPTEDFIELDSRESOURCES, ["name","tags","model","description","manufacturer"]).

%% Fields for data-points
-define(ACCEPTEDFIELDSDATAPOINTS, ["streamid","timestamp","value"]).