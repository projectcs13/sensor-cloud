%% @author Tommy Mattsson [www.csproj13.student.it.uu.se]
%% @copyright [Copyright information]
%% @version 1.0
%% @doc == Configuration manipulation module ==
%% @end
-module(config).
-include("debug.hrl").
-export([start/0]).

%% @doc
%% Runs the configuration mechanism for the entire project. Sets proper configuration 
%% for configuration files in libraries.
%% @end
start() ->
    case config_sanity_check() of
	ok ->
	    ;
	error ->
	    erlang:exit(config_file_error)
    end,
    %% erlastic_search_config(),
    %% rabbit_mq_config(),
    %% webmachine_config(),
    init:stop().

config_sanity_check() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    File = CWD ++ "/config/engine.config",
	    case file:open(File, [read]) of
		{ok, Fd} ->
		    case io:scan_erl_exprs(Fd, "") of
			{ok, List, _EndLine} ->
			    Fun = fun({',',_},Acc) -> 
					  Acc;
				     ({'{',_},Acc) ->
					  Acc;
				     ({'}',_},Acc) ->
					  Acc;
				     ({'[',_},Acc) ->
					  Acc;
				     ({']',_},Acc) ->
					  Acc;
				     ({dot,_},Acc) ->
					  Acc;
				     ({atom,_,engine},Acc) ->
					  Acc;
				     ({_,_,X},Acc) ->
					  [X|Acc]
				  end,
			    FilterAndPair = pair(lists:foldr(Fun, [], List)),
			    Fun2 = fun({ConfOpt, ConfVal}) ->
					   case application:get_env(engine, ConfOpt) of
					       {ok, ConfVal} ->
						   ?DEBUG(lists:concat(["Config option '", ConfOpt, "' is defined"]));
					       {ok, _} ->
						   ?ERROR(lists:concat(["Config option '", ConfOpt, "' is defined, but has wrong value in relation to the config file"]));
					       undefined ->
						   ?ERROR(lists:concat(["Config option '", ConfOpt, "' is not defined"]))
					   end
				   end,
			    lists:foreach(Fun2, FilterAndPair);
			_ ->
			    ?ERROR("Unable to read from config file"),
			    error
		    end;
		_ ->
		    ?ERROR("Unable to get current working directory, aborting config sanity check"),
		    error
	    end;
	_ ->
	    ?ERROR("Unable to open config file, aborting configuration mechanismt"),
	    error
    end.

pair(List) ->
    pair(List, []).
pair([], Acc) ->
    lists:reverse(Acc);
pair([A, B| Tl], Acc) ->
    pair(Tl, [{A,B} | Acc]).

%% @doc
%% Runs the configuration mechanism for the elastic search and erlastic libraries
%% @end
-spec erlastic_search_config() -> ok | error.
erlastic_search_config() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    File = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
	    Lines = read_file_lines(File),
	    StrippedLines = lists:map(fun string:strip/1, Lines),
	    Fun = fun("host"++Line, Acc) ->
			  NewLine = case application:get_env(engine, es_ip) of
					{ok, Ip} ->				  
					    "host = \""++ Ip ++ "\":: string(),\n";
					_ ->
					    ?ERROR("Environment variable 'es_ip' is not set"),
					    "host"++Line
				    end,
			  [NewLine | Acc];
		     ("port"++Line, Acc) ->
			  NewLine = case application:get_env(engine, es_port) of
					{ok, Port} ->			
					    "port = "++ integer_to_list(Port) ++ ":: integer(),\n";
					_ ->
					    ?ERROR("Environment variable 'es_port' is not set"),
					    "port"++Line
				    end,
			  [NewLine | Acc];
		     (Line, Acc) ->
			  [Line | Acc]
		  end,
	    NewLines = lists:foldr(Fun, [], StrippedLines),
	    write_lines(File, NewLines),
	    ?DEBUG("Finished configuring elastic_search config options");
	_ ->
	    ?ERROR("Unable to get current working directory, aborting config operations for elastic_search"),
	    error
    end.

%% @doc
%% Runs the configuration mechanism for the rabbit_mq library
%% @end
-spec rabbit_mq_config() -> ok | error.
rabbit_mq_config() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    File = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
	    ?DEBUG(file:read_file_info(File));
	_ ->
	    ?ERROR("Unable to get current working directory, aborting config operations for rabbit_mq")
    end.

%% @doc
%% Runs the configuration mechanism for the webmachine library
%% @end
-spec webmachine_config() -> ok | error.
webmachine_config() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    File = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
	    ?DEBUG(file:read_file_info(File));
	_ ->
	    ?ERROR("Unable to get current working directory, aborting config operations for webmachine")
    end.



%% @doc
%% Read all lines from a file.
%% @end
-spec read_file_lines(File::string()) -> [string()] | error.
read_file_lines(File) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Lines = read_file_lines(Fd, []),
	    file:close(Fd),
	    Lines;
	_ ->
	    ?ERROR("Unable to open file: "++File),
	    error
    end.

%% @doc
%% Read all lines from a file. Used by read_file_lines/1
%% @end
-spec read_file_lines(Fd::pid(), Acc::list()) -> [string()].
read_file_lines(Fd, Acc) ->
    case file:read_line(Fd) of
	eof ->
	    lists:reverse(Acc);
	{ok, Line} ->
	    read_file_lines(Fd, [Line | Acc])
    end.

%% @doc
%% Writes all lines from a file. If the file exists then all of the old content is removed and Lines will be inserted instead.
%% @end
-spec write_lines(Fd::pid(), Lines::[string()]) -> ok | error.
write_lines(File, Lines) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    file:write(Fd, Lines),
	    file:close(Fd);
	_ ->
	    ?ERROR("Unable to open file: "++File),
	    error
    end.
