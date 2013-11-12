%% @author Tommy Mattsson [www.csproj13.student.it.uu.se]
%% @copyright [Copyright information]
%% @version 1.0
%% @doc == Configuration manipulation module ==
%% @end
-module(config).
-include("debug.hrl").
-export([start/0]).

start() ->
    %% {ok, RootPath} = file:get_cwd(),
    %% FileName = RootPath ++ "/erlastic_search.hrl",
    %% Lines = read_file_lines(FileName),
    %% NewLines = config_lines(Lines),
    %% write_lines(FileName, NewLines),
    elastic_search_config(),
    init:stop().


elastic_search_config() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    FileName = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
	    ?DEBUG(file:read_file_info(FileName));
	_ ->
	    error
    end.

rabbit_mq_config() ->
    true.

webmachine_config() ->
    true.



read_file_lines(File) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Lines = read_file_lines(Fd, []),
	    file:close(Fd),
	    Lines;
	_ ->
	    error
    end.
    
read_file_lines(Fd, Acc) ->
    case file:read_line(Fd) of
	eof ->
	    lists:reverse(Acc);
	{ok, Line} ->
	    read_file_lines(Fd, [Line | Acc])
    end.

write_lines(File, Lines) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    file:write(Fd, Lines),
	    file:close(Fd);
	_ ->
	    error
    end.
