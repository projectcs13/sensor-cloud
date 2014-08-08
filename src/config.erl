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
-spec start() -> no_return_value.
start() ->
    case engine_config_check() of
	ok ->
	    ?DEBUG("Engine config file check passed");
	error ->
	    ?ERROR("Engine config file check failed. Aborting config mechanism."),
	    erlang:exit(engine_config_file_error)
    end,
    case elastic_search_config_check() of
	ok ->
	    ?DEBUG("Elastic_search config file check passed");
	error ->
	    ?ERROR("Elastic_search config file check failed. Aborting config mechanism."),
	    erlang:exit(elastic_config_file_error)
    end,
    case erlastic_search_config_check() of
	ok ->
	    ?DEBUG("Erlastic_search config file check passed");
	error ->
	    ?ERROR("Erlastic_search config file check failed. Aborting config mechanism."),
	    erlang:exit(erlastic_config_file_error)
    end,
    case rabbitmq_config_check() of
	ok ->
	    ?DEBUG("RabbitMQ config file check passed");
	error ->
	    ?ERROR("RabbitMQ config file check failed. Aborting config mechanism."),
	    erlang:exit(rabbitmq_config_file_error)
    end,
    
    engine_and_nodejs_config(),
    elastic_search_config(),
    erlastic_search_config(),
    rabbit_mq_config(),

    %% Webmachine configuration settings was altered in src/engine_sup.erl to use the the application
    %% environment variables which is loaded when the project is started. 
    %% To set webmachine config settings it is only the config/engine.config which needs to be edited.
    init:stop().

%% @doc
%% Check validity of the projects config file. Make sure that all environment variables is loaded.
%% @end
-spec engine_config_check() -> ok | error.
engine_config_check() ->
    case file:get_cwd() of
	{ok, CWD} ->
	    File = CWD ++ "/config/engine.config",
	    case file:open(File, [read]) of
		{ok, Fd} ->
		    case io:scan_erl_exprs(Fd, "") of
			{ok, List, _EndLine} ->
			    %% The use of io:scan_erl_exprs produces a list where I can just filter out
			    %% most of the terms.
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
						   ok;
					       {ok, _} ->
						   ?ERROR(lists:concat(["Config option '", ConfOpt, 
									"' is defined, but has wrong value "
									"in relation to the config file"])),
						   error;
					       undefined ->
						   ?ERROR(lists:concat(["Config option '", ConfOpt, "' is not defined"])),
						   error
					   end
				   end,
			    CheckConfig = lists:map(Fun2, FilterAndPair),
			    case lists:all(fun(X) -> X =/= error end, CheckConfig) of
				true ->
				    ok;
				false ->
				    error
			    end;
			_ ->
			    ?ERROR("Unable to read from config file"),
			    error
		    end;
		_ ->
		    ?ERROR("Unable to get current working directory"),
		    error
	    end;
	_ ->
	    ?ERROR("Unable to open config file"),
	    error
    end.

%% @doc
%% Check validity of elastic_search config file
%% @end
-spec elastic_search_config_check() -> ok | error.
elastic_search_config_check() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/elasticsearch-1.1.2/config/elasticsearch.yml",
    case file:read_file_info(File) of
	{ok, _FileInfo}->
	    ok;
	{error, _Error} ->
	    ?ERROR("Elastic search config file NOT FOUND"),
	    error
    end.


%% @doc
%% Check validity of erlastic_search config file
%% @end
-spec erlastic_search_config_check() -> ok | error.
erlastic_search_config_check() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
    case file:read_file_info(File) of
	{ok, _FileInfo}->
	    ok;
	{error, _Error} ->
	    ?ERROR("Erlastic search config file NOT FOUND"),
	    error
    end.

%% @doc
%% Check validity of RabbitMQ config file
%% @end
-spec rabbitmq_config_check() -> ok | error.
rabbitmq_config_check() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/rabbitmq-server/scripts/rabbitmq-defaults",
    case file:read_file_info(File) of
	{ok, _FileInfo} ->
	    ok;
	{error, _Error} ->	    
	    ?ERROR("RabbitMQ server config file NOT FOUND"),
	    error
    end,
    File2 = CWD ++ "/lib/rabbitmq-erlang-client/include/amqp_client.hrl",
    case file:read_file_info(File2) of
	{ok, _FileInfo2} ->
	    ok;
	{error, _Error2} ->	    
	    ?ERROR("RabbitMQ erlang client config file NOT FOUND"),
	    error
    end.

%% @doc
%% Runs the configuration mechanism for the application
%% @end
-spec engine_and_nodejs_config() -> ok.
engine_and_nodejs_config() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/scripts/sensec.sh",
    Lines = lib_file:read_file_lines(File),
    NewLines = [case X of
		    OldLine = "LOG_DIR"++_Line ->
			case application:get_env(engine, engine_log_dir) of
			    undefined ->
				?DEBUG("Application log dir not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				lib_file:ensure_dir_exists(Value),
				"LOG_DIR=" ++ Value ++ "\n"
			end;
		    OldLine = "LOG_JS_DIR"++_Line ->
			case application:get_env(engine, nodejs_log_dir) of
			    undefined ->
				?DEBUG("NodeJS log dir not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				lib_file:ensure_dir_exists(Value),
				"LOG_JS_DIR=" ++ Value ++ "\n"
			end;
		    OldLine = "ES_PORT"++_Line ->
			case application:get_env(engine, es_port) of
			    undefined ->
				?DEBUG("Elastic search port option not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				"ES_PORT=" ++ integer_to_list(Value) ++ "\n"
			end;
		    X ->
			X
		end || X <- Lines],  
     case NewLines of
	Lines ->
	    ok; %% Nothing in the configuration file was changed so we don't write to the file
	_ ->
	    lib_file:write_file_lines(File, NewLines)
    end,
    ?DEBUG("Finished configuring application config options").


%% @doc
%% Runs the configuration mechanism for the elastic search
%% @end
-spec elastic_search_config() -> ok.
elastic_search_config() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/elasticsearch-1.1.2/config/elasticsearch.yml",
    Lines = lib_file:read_file_lines(File),
    FunStrip = fun(X) -> NoComment = string:strip(X, left, $#),
			 string:strip(NoComment, left) 
	       end, 
    %% Take away comment characters & spaces from the beginning of the line
    StrippedLines = [FunStrip(X) || X <- Lines],
    Fun = fun(OldLine = "network.host"++_Line, Acc) ->
		  case application:get_env(engine, es_ip) of
		      undefined ->
			  ?DEBUG("Elastic search ip option not defined. Using default value."),
			  [OldLine|Acc];
		      {ok, Value} ->
			  ["network.host: " ++ Value ++ "\n"|Acc]
		  end;
	     (OldLine = "http.port"++_Line, Acc) ->
		  case application:get_env(engine, es_port) of
		      undefined ->
			  ?DEBUG("Elastic search port option not defined. Using default value."),
			  [OldLine|Acc];
		      {ok, Value} ->
			  ["http.port: " ++ integer_to_list(Value) ++ "\n"|Acc]
		  end;
	     (OldLine = "path.data"++_Line, Acc) ->
		  case application:get_env(engine, es_db_dir) of
		      undefined ->
			  ?DEBUG("Elastic search data directory option not defined. Using default value."),
			  [OldLine|Acc];
		      {ok, Value} ->
			  DataLine1 = "path.data: " ++ Value ++ "\n",
			  case lists:member(DataLine1, Acc) of
			      true ->
				  case application:get_env(engine, es_db_extra_dir) of
				      undefined ->
					  ?DEBUG("Elastic search extra data directory option not defined. "
						 "Not using extra data directory"),
					  [OldLine|Acc];
				      {ok, ""} ->
					  ?DEBUG("Elastic search extra data directory option defined as empty string. "
						 "Not using extra data directory"),
					  ["# "++OldLine|Acc];
				      {ok, Value2} ->
					  ["path.data: " ++ Value2 ++ "\n"|Acc]
				  end;
			      false ->
				  lib_file:ensure_dir_exists(Value),
				  [DataLine1|Acc]
			  end
		  end;
	     (OldLine = "path.logs"++_Line, Acc) ->
		  case application:get_env(engine, es_log_dir) of
		      undefined ->
			  ?DEBUG("Elastic search log directory option not defined. Using default value."),
			  [OldLine|Acc];
		      {ok, Value} ->
			  lib_file:ensure_dir_exists(Value),
			  ["path.logs: " ++ Value ++ "\n"|Acc]
		  end;
	     (OldLine = "cluster.name"++_Line, Acc) ->
		  case application:get_env(engine, es_cluster_name) of
		      undefined ->
			  ?DEBUG("Elastic search cluster name option not defined. Using default value."),
			  [OldLine|Acc];
		      {ok, Value} ->
			  ["cluster.name: " ++ Value ++ "\n"|Acc]
		  end;
	     (Line, Acc) ->
		  case lists:member(Line, Lines) of
		      true ->
			  [Line|Acc];
		      false ->
			  NewLine = "# "++ Line,
			  [NewLine|Acc]
		  end
	  end,
    NewLines = lists:reverse(lists:foldl(Fun, [], StrippedLines)),
    case NewLines of
	Lines ->
	    ok; %% Nothing in the configuration file was changed so we don't write to the file
	_ ->
	    lib_file:write_file_lines(File, NewLines)
    end,
    ?DEBUG("Finished configuring elastic_search config options").


%% @doc
%% Runs the configuration mechanism for the erlastic libraries
%% @end
-spec erlastic_search_config() -> ok.
erlastic_search_config() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/erlastic_search/include/erlastic_search.hrl",
    Lines = lib_file:read_file_lines(File),
    FunStrip = fun(X) -> string:strip(X, left) end, 
    %% Take away comment characters & spaces from the beginning of the line
    StrippedLines = [FunStrip(X) || X <- Lines],
    NewLines = [case X of
		    OldLine = "host"++_Line ->
			case application:get_env(engine, es_ip) of
			    undefined ->
				?DEBUG("Elastic search ip option not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				"host = \""++ Value ++ "\":: string(),\n"
			end;
		    OldLine = "port"++_Line ->
			case application:get_env(engine, es_port) of
			    undefined ->
				?DEBUG("Elastic search port option not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				"port = "++ integer_to_list(Value) ++ " :: integer(),\n"
			end;
		    X ->
			X
		end || X <- StrippedLines], 
    case NewLines of
	Lines ->
	    ok; %% Nothing in the configuration file was changed so we don't write to the file
	_ ->
	    lib_file:write_file_lines(File, NewLines)
    end,
    ?DEBUG("Finished configuring erlastic_search config options").


%% @doc
%% Runs the configuration mechanism for the rabbit_mq library
%% @end
-spec rabbit_mq_config() -> ok.
rabbit_mq_config() ->
    {ok, CWD} = file:get_cwd(),
    File = CWD ++ "/lib/rabbitmq-server/scripts/rabbitmq-defaults",
    Lines = lib_file:read_file_lines(File),
    FunStrip = fun(X) -> string:strip(X, left) end, 
    %% Take away comment characters & spaces from the beginning of the line
    StrippedLines = [FunStrip(X) || X <- Lines],
    NewLines = [case X of
		    OldLine = "LOG_BASE"++_Line ->
			case application:get_env(engine, rabbit_mq_log_dir) of
			    undefined ->
				?DEBUG("RabbitMQ log directory option not defined. Using default value."),
				OldLine;
			    {ok, Value} ->
				lib_file:ensure_dir_exists(Value),
				"LOG_BASE=" ++ Value ++ "\n"
			end;
		    X ->
			X
		end || X <- StrippedLines],
    case NewLines of
	Lines ->
	    ok; %% Nothing in the configuration file was changed so we don't write to the file
	_ ->
	    lib_file:write_file_lines(File, NewLines)
    end, 
    ?DEBUG("Finished configuring rabbit_mq server config options"),
    

    File2 = CWD ++ "/lib/rabbitmq-erlang-client/include/amqp_client.hrl",
    Lines2 = lib_file:read_file_lines(File2),
    %% Take away comment characters & spaces from the beginning of the line
    StrippedLines2 = [FunStrip(X) || X <- Lines2],
    Fun = fun(OldLine = "port"++_Line, Acc) ->
		  DefaultPort = "port = 5672,\n",
		  case application:get_env(engine, rabbit_mq_port) of
		      undefined ->
			  ?DEBUG("RabbitMQ Server port option not defined. Using default value."),
			  [DefaultPort|Acc];
		      {ok, Value} ->
			  DataLine = "port = " ++ integer_to_list(Value) ++ ",\n",
			  case lists:member(DataLine, Acc) orelse lists:member(DefaultPort, Acc) of
			      true ->
				  [OldLine|Acc];
			      false ->
				  [DataLine|Acc]
			  end
		  end;
	     (OldLine = "host"++_Line, Acc) ->
		  DefaultHost = "host = \"localhost\",\n",
		  case application:get_env(engine, rabbit_mq_ip) of
		      undefined ->
			  ?DEBUG("RabbitMQ Server ip option not defined. Using default value."),
			  [DefaultHost|Acc];
		      {ok, Value} ->
			  DataLine = "host = \"" ++ Value ++ "\",\n",
			  case lists:member(DataLine, Acc) orelse lists:member(DefaultHost, Acc) of
			      true ->
				  [OldLine|Acc];
			      false ->
				  [DataLine|Acc]
			  end
		  end;
	     (Line, Acc) ->
		  [Line|Acc]
	  end,
    NewLines2 = lists:reverse(lists:foldl(Fun, [], StrippedLines2)),
    case NewLines2 of
	Lines2 ->
	    ok; %% Nothing in the configuration file was changed so we don't write to the file
	_ ->
	    lib_file:write_file_lines(File2, NewLines2)
    end, 
    ?DEBUG("Finished configuring rabbit_mq erlang client config options").



pair(List) ->
    pair(List, []).
pair([], Acc) ->
    lists:reverse(Acc);
pair([A, B| Tl], Acc) ->
    pair(Tl, [{A,B} | Acc]).
