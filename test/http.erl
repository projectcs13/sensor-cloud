%% @author Tommy Mattsson <tommy.mattsson@gmail.com>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == http ==
%% This module contains several functions for making http request with webmachine
%% in the module triggers which is done by calling the webbmachine.
%%
%% @end
-module(http).
-export([delete/1, get/1, post/2, put/2]).

delete(Url) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(delete,{Url,[]},[],[]),
    {Status, Body}.

get(Url) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(get,{Url, []}, [], []),
    {Status, Body}.



post(Url, Request) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(post,{Url,[],"application/json",Request},[],[]),
    {Status, Body}.

put(Url, Request) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(put,{Url,[],"application/json",Request},[],[]),
    {Status, Body}.
