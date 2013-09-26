
%% @author Tholsgård Gabriel
%%   [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == db_api definitions ==
%% This header contains several definitions for
%% how data will be retrieved from the mnesia database.
%%
%% @end

%% mnesia table record for streams:
-record(streams, {id, type, latitude, longitude, description, public_access,
				  public_search, frozen, history_size, last_updated,
				  secret_key, owner_id, resource_id, version}).