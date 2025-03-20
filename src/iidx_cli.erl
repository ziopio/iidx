%@doc
% Utility module for general purpose cli mechanics
%
-module(iidx_cli).

-export([info/1]).
-export([info/2]).
-export([warn/1]).
-export([warn/2]).
-export([abort/1]).
-export([abort/2]).

%--- API -----------------------------------------------------------------------

info(Text) -> info(Text, []).

info(Format, Args) ->
    io:format(Format ++ "~n", Args).

warn(Text) -> warn(Text, []).

warn(Format, Args) ->
    io:format("~s~n", [color:yellow(io_lib:format(Format, Args))]).

abort(Text) -> abort(Text, []).

abort(Format, Args) ->
    io:format("~s~n", [color:red(io_lib:format(Format, Args))]),
    erlang:halt(1).
