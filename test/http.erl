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

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 
%% Makes an http delete request to an url.
%% @end
-spec delete(Url::string()) -> {integer(), string()}.
delete(Url) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(delete,{Url,[]},[],[]),
    {Status, Body}.

%% @doc 
%% Makes an http get request to an url.
%% @end
-spec get(Url::string()) -> {integer(), string()}.
get(Url) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(get,{Url, []}, [], []),
    {Status, Body}.

%% @doc 
%% Makes an http post request to an url with specific data.
%% @end
-spec post(Url::string(), Request::string()) -> {integer(), string()}.
post(Url, Request) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(post,{Url,[],"application/json",Request},[],[]),
    {Status, Body}.

%% @doc 
%% Makes an http put request to an url with specific data.
%% @end
-spec put(Url::string(), Request::string()) -> {integer(), string()}.
put(Url, Request) ->
    {ok,{{_Vsn,Status,_Reason},_Hdrs,Body}} = httpc:request(put,{Url,[],"application/json",Request},[],[]),
    {Status, Body}.
