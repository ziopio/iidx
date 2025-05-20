%@doc
% Utility module for general purpose cli mechanics
%
-module(iidx_cli).

-export([success/1]).
-export([success/2]).
-export([info/1]).
-export([info/2]).
-export([warn/1]).
-export([warn/2]).
-export([abort/1]).
-export([abort/2]).
-export([read_file/1]).
-export([write_file/2]).
-export([find_files/2]).
-export([exec/2]).
-export([get_temp_dir/0]).
-export([assert_path_exists/1]).
%--- API -----------------------------------------------------------------------

success(Text) -> success(Text, []).

success(Format, Args) ->
    format(Format, green, Args).

info(Text) -> info(Text, []).

info(Format, Args) ->
    format(Format, Args).

warn(Text) -> warn(Text, []).

warn(Format, Args) ->
     format(Format, yellow, Args).

abort(Text) -> abort(Text, []).

abort(Format, Args) ->
    format(Format, red, Args),
    erlang:halt(1).

read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Binary;
        {error, Reason} ->
            iidx_cli:abort("Cannot read file ~s: ~p", [Filename, Reason])
    end.

write_file(Filename, Binary) ->
    iidx_cli:info("Writing file: ~s", [Filename]),
    case file:write_file(Filename, Binary) of
        ok ->
            ok;
        {error, Reason} ->
            iidx_cli:abort("Cannot write file ~s for reason ~p", [Filename,
                                                                  Reason])
    end.

find_files(BMSfolder, Wildcard) ->
    FullWildcard = binary_to_list(filename:join(BMSfolder, Wildcard)),
    case filelib:wildcard(FullWildcard) of
        [] ->
            iidx_cli:abort("Cannot find ~p files in folder ~p", [Wildcard, BMSfolder]);
        Files ->
            Files
    end.

assert_path_exists(Path) ->
    case filelib:is_dir(Path) of
        true -> ok;
        false -> iidx_cli:abort("Path is not a directory: ~p",[Path])
    end.

exec(Program, Args) ->
    PortOptions = [
        binary,
        {args, Args},
        exit_status,
        stderr_to_stdout,
        hide
    ],
    Port = erlang:open_port({spawn_executable, Program}, PortOptions),
    collect_port_data(Port, <<>>).

get_temp_dir() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("TEMP") of
                false -> os:getenv("TMP");
                Temp -> Temp
            end;
        _ ->
            case os:getenv("TMPDIR") of
                false -> "/tmp";
                TmpDir -> TmpDir
            end
    end.

%--- Internal ------------------------------------------------------------------

collect_port_data(Port, Acc) ->
    receive
        {Port, {data, {eol, Data}}} ->
            collect_port_data(Port, <<Acc/binary, Data/binary>>);
        {Port, {data, {noeol, Data}}} ->
            collect_port_data(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            Acc;
        {Port, {exit_status, Status}} ->
            iidx_cli:abort("Port exited with status ~p", [Status])
    end.

format(Format, Args) ->
    io:format(Format ++ "~n", Args).

format(Format, Color, Args) ->
    io:format("~s~n", [color:Color(io_lib:format(Format, Args))]).
