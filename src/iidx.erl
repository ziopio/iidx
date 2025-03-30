-module(iidx).

-define(REQUIRED_OTP_VERSION, 27).

% Callbacks
-export([main/1]).

%--- API -----------------------------------------------------------------------

% @doc Main CLI entry point.
main(Args) ->
    check_otp_version(?REQUIRED_OTP_VERSION),
    try
        argparse:run(Args, cli(), #{progname => ?MODULE})
    catch
        Class:Reason:Stacktrace ->
            cli_abort(Class, Reason, Stacktrace)
    end.


%--- Internals -----------------------------------------------------------------

cli() ->
    #{
        arguments => [
            #{
                name => help,
                long => "-help",
                short => $h,
                help => "print this help message"
            }
        ],
        commands => #{
            "convert" => iidx_convert:cli()
        },
        handler => fun(_) -> print_help() end
    }.

print_help() ->
    iidx_cli:info(argparse:help(cli(), #{progname => ?MODULE})).

check_otp_version(Version) ->
    check_otp_version(
        Version,
        list_to_integer(erlang:system_info(otp_release))
    ).

check_otp_version(Desired, Actual) when Desired > Actual ->
    iidx_cli:abort("OTP version ~p too old. At least ~p required.", [
        Actual,
        Desired
    ]);
check_otp_version(_, _) ->
    ok.

cli_abort(Class, Reason, Stacktrace) ->
    erlang:raise(Class, Reason, Stacktrace).
