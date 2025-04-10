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

%--- Internal ------------------------------------------------------------------

format(Format, Args) ->
    io:format(Format ++ "~n", Args).

format(Format, Color, Args) ->
    io:format("~s~n", [color:Color(io_lib:format(Format, Args))]).
