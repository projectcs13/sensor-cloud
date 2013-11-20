%% @author Tommy Mattsson [www.csproj13.student.it.uu.se]
%% @copyright [Copyright information]
%% @version 1.0
%% @doc == Library for manipulating files and directories ==
%% @end
-module(lib_file).
-include("debug.hrl").
-export([ensure_dir_exists/1, read_file_lines/1, write_file_lines/2]).

%% @doc
%% Ensures a directory exists.
%% @end
-spec ensure_dir_exists(Dir::string()) -> ok | error.
ensure_dir_exists(AbsoluteDir = "/"++_Dir) ->
    User = os:cmd("echo $USER")--"\n",
    case User of
	"root" ->
	    os:cmd("mkdir -p "++AbsoluteDir),
	    ok;
	User ->
	    case re:run("^/home/"++User, AbsoluteDir, [{capture, none}]) of
		nomatch ->
		    case file:list_dir(AbsoluteDir) of
			{ok, _FileList} ->
			    ?DEBUG("Directory '" ++ AbsoluteDir ++ "' exists."),
			    ok;
			{error, _Reason} ->
			    ?DEBUG("Cannot create directory '"++AbsoluteDir++"' without sudo rights. Rerun with sudo access"),
			    error
		    end;
		match ->
		    os:cmd("mkdir -p " ++ AbsoluteDir),
		    ok
	    end
    end;
ensure_dir_exists(RelativeDir) ->    
    os:cmd("mkdir -p " ++ RelativeDir),
    ok.




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
-spec write_file_lines(Fd::pid(), Lines::[string()]) -> ok | error.
write_file_lines(File, Lines) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    file:write(Fd, Lines),
	    file:close(Fd);
	_ ->
	    ?ERROR("Unable to open file: "++File),
	    error
    end.
