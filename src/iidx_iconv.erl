-module(iidx_iconv).

-export([convert/3]).

-compile([no_native]).

-on_load(load_nif/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================
load_nif() ->
    NifFile = get_nif_filename(),
    FullFileName = case filelib:is_dir(code:priv_dir(iidx)) of
        false ->
            % The app is zipped into a file, probably an escript...
            Nif = filename:join(["iidx", "priv", NifFile]),
            extract_from_escript(Nif);
        true ->
            filename:join(code:priv_dir(iidx), NifFile)
    end,
    % name without file extension
    NifName = filename:rootname(FullFileName),
    case erlang:load_nif(NifName, 0) of
        ok ->
            ok;
        {error, {Reason, Txt}} ->
            io:format("~s~n", [Txt]),
            error({nif_load_error, Reason})
    end.

-spec convert(iodata(), iodata(), iodata()) -> binary().
convert(_From, _To, _String) ->
    erlang:nif_error(nif_not_loaded).

get_nif_filename() ->
    Ext = case os:type() of
        {win32, _} -> ".dll";
        _ -> ".so"
    end,
    "iidx_iconv" ++ Ext.

extract_from_escript(ZippedFile) ->
    EscriptPath = escript:script_name(),
    {ok, Escript} = escript:extract(EscriptPath, []),
    [_Shebang, _Comment, _EmuArgs, Body] = Escript,
    {archive, ArchiveBin} = Body,
    GetFun = take_file_from_zip(ZippedFile),
    Result = zip:foldl(GetFun, undefined, {EscriptPath, ArchiveBin}),
    FileName = filename:basename(ZippedFile),
    case Result of
        {ok, NifBinary} ->
            TmpDir = iidx_cli:get_temp_dir(),
            TmpNifName = filename:join(TmpDir, FileName),
            case file:write_file(TmpNifName, NifBinary) of
                ok ->
                    TmpNifName;
                {error, Reason} ->
                    error({file_extraction_failed, Reason})
            end;
        undefined ->
            error({file_not_in_escript, ZippedFile})
    end.

take_file_from_zip(Filename) ->
    fun(File, _, GetBin, Acc) ->
        case File of
            Filename -> GetBin();
            _ ->
                Acc
        end
    end.

%%%===================================================================
%%% Unit tests
%%%===================================================================

-ifdef(TEST).

utf8_to_koi8r_test() ->
    ?assertEqual(
       <<212,197,211,212>>,
       iidx_iconv:convert("utf-8", "koi8-r", <<209,130,208,181,209,129,209,130>>)).

koi8r_to_cp1251_test() ->
    ?assertEqual(
       <<242,229,241,242>>,
       iidx_iconv:convert("koi8-r", "cp1251", <<212,197,211,212>>)).

shift_jis_test() ->
    ?assertEqual(
       <<130, 160, 130, 161>>,
       iidx_iconv:convert("utf-8", "shift_jis", <<227, 129, 130, 227, 129, 131>>)).

shift_jis_to_utf8_test() ->
    ?assertEqual(
       <<227, 129, 130, 227, 129, 131>>,
       iidx_iconv:convert("shift_jis", "utf-8", <<130, 160, 130, 161>>)).

wrong_encoding_test() ->
    ?assertEqual(
       <<1,2,3,4,5>>,
       iidx_iconv:convert("wrong_encoding_from",
                          "wrong_encoding_to",
                          <<1,2,3,4,5>>)).

-endif.
