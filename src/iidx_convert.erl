-module(iidx_convert).


-export([cli/0]).


cli() -> #{
    arguments => [
        #{
            name => bms_file
        }
    ],
    help => "Converts a BMS chart into a .1 chart compatible with IIDX",
    handler => fun convert/1
}.

convert(#{bms_file := BMSfile}) ->
    assert_file_exists(BMSfile).

assert_file_exists(Path) ->
    case filelib:is_regular(Path) of
        true -> ok;
        false -> iidx_cli:abort("File does not exists: ~p",[Path])
    end.
