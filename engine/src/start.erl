-module(start).
-export([init/0, doc/0]).

init() ->
    poff.

doc() ->
    edoc:application(website, '.', [{packages, false},{private, true}]),
    init:stop().
