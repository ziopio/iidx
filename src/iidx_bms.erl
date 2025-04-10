-module(iidx_bms).

-export([decode/1]).

%% BMS Channel classification
-define(BGM, "01").
-define(TEMPO_CHANGE, "03").
-define(BGA, "04").
-define(POOR_BITMAP, "06").
% BME extended channels (beatmaniaIIDX)
-define(BGA_LAYER, "07"). % image overlayng BGA
-define(EXT_BPM,   "08"). % reference to #BPMxx n

% IIDX Keys mapping for BMS
% Key -> Channel
% Keys are numbered left to rigth
%         2   4   6
%  ⊙
%       1   3   5   7
% P1
-define(P1_SCRATCH,      "16").
-define(P1_KEY_1,        "11").
-define(P1_KEY_2,        "12").
-define(P1_KEY_3,        "13").
-define(P1_KEY_4,        "14").
-define(P1_KEY_5,        "15").
-define(P1_KEY_6,        "18").
-define(P1_KEY_7,        "19").
-define(P1_SCRATCH_LONG, "56").
-define(P1_KEY_1_LONG,   "51").
-define(P1_KEY_2_LONG,   "52").
-define(P1_KEY_3_LONG,   "53").
-define(P1_KEY_4_LONG,   "54").
-define(P1_KEY_5_LONG,   "55").
-define(P1_KEY_6_LONG,   "58").
-define(P1_KEY_7_LONG,   "59").
% P2
-define(P2_SCRATCH,      "26").
-define(P2_KEY_1,        "21").
-define(P2_KEY_2,        "22").
-define(P2_KEY_3,        "23").
-define(P2_KEY_41,       "24").
-define(P2_KEY_5,        "25").
-define(P2_KEY_6,        "28").
-define(P2_KEY_7,        "29").
-define(P2_SCRATCH_LONG, "66").
-define(P2_KEY_1_LONG,   "61").
-define(P2_KEY_2_LONG,   "62").
-define(P2_KEY_3_LONG,   "63").
-define(P2_KEY_4_LONG,   "64").
-define(P2_KEY_5_LONG,   "65").
-define(P2_KEY_6_LONG,   "68").
-define(P2_KEY_7_LONG,   "69").

%--- API -----------------------------------------------------------------------

decode(Binary) ->
    Lines = binary:split(Binary, <<"\r\n">>, [global]),
    lists:foldl(fun decode_line/2, #{}, Lines).

%--- Internals -----------------------------------------------------------------

decode_line(<<>>, S) -> S;
decode_line(<<"ï»¿">>, S) -> S; % UTF-8 BOM (ignored)
decode_line(<<"*---------------------- HEADER FIELD">>, S) ->
    S;
decode_line(<<"#PLAYER 1">>, S) ->
    mapz:deep_put([header, player], single, S);
decode_line(<<"#PLAYER 2">>, S) ->
    mapz:deep_put([header, player], couple, S);
decode_line(<<"#PLAYER 3">>, S) ->
    mapz:deep_put([header, player], double, S);
decode_line(<<"#GENRE ", Genre/binary>>, S) ->
    mapz:deep_put([header, genre], Genre, S);
decode_line(<<"#TITLE ", Title/binary>>, S) ->
    mapz:deep_put([header, title], Title, S);
decode_line(<<"#ARTIST ", Artist/binary>>, S) ->
    mapz:deep_put([header, artist], Artist, S);
decode_line(<<"#SUBARTIST ", Subject/binary>>, S) ->
    [Role, SubArtist] = binary:split(Subject, <<":">>),
    mapz:deep_put([header, subartist, Role], SubArtist, S);
decode_line(<<"#BPM ", BPM/binary>>, S) ->
    Value = try binary_to_integer(BPM)
        catch
          error:badarg -> binary_to_float(BPM)
        end,
    mapz:deep_put([header, bpm], Value, S);
decode_line(<<"#BPM", NUM:2/binary, " ", BPM/binary>>, S) ->
    Value = try binary_to_integer(BPM)
        catch
          error:badarg -> binary_to_float(BPM)
        end,
    mapz:deep_put([header, extended_bpm, NUM], Value, S);
decode_line(<<"#MIDIFILE ", MidiFile/binary>>, S) ->
    mapz:deep_put([header, midifile], MidiFile, S);
decode_line(<<"#PLAYLEVEL ", PlayLevel/binary>>, S) ->
    mapz:deep_put([header, playlevel], PlayLevel, S);
decode_line(<<"#DIFFICULTY ", Difficulty/binary>>, S) ->
    mapz:deep_put([header, difficulty], Difficulty, S);
decode_line(<<"#RANK ", Rank/binary>>, S) ->
    mapz:deep_put([header, rank], Rank, S);
decode_line(<<"#STAGEFILE ", StageFile/binary>>, S) ->
    mapz:deep_put([header, stagefile], StageFile, S);
decode_line(<<"#LNTYPE ", N/binary>>, S) ->
    mapz:deep_put([header, long_note_type], N, S);
decode_line(<<"#TOTAL ", N/binary>>, S) ->
    mapz:deep_put([header, gauge_total], N, S);
decode_line(<<"#WAV", NUM:2/binary, " ", FileName/binary>>, S) ->
    mapz:deep_put([header, wav, NUM], FileName, S);
decode_line(<<"#BMP", NUM:2/binary, " ", Bitmap/binary>>, S) ->
    mapz:deep_put([header, bitmap, NUM], Bitmap, S);
decode_line(<<"*---------------------- EXPANSION FIELD">>, S) ->
    S;
decode_line(<<"*---------------------- MAIN DATA FIELD">>, S) ->
    S;
decode_line(<<"#", Track:3/binary, Channel:2/binary, ":", Message/binary>>, S) ->
    Messages = mapz:deep_get([data, messages], S, []),
    NewMsg = {Track, decode_channel(Channel), decode_message(Message)},
    mapz:deep_put([data, messages], Messages ++ [NewMsg], S);
decode_line(Line, S) ->
    iidx_cli:warn("Unhandled ~p...", [Line]),
    S.

decode_channel(<<?BGM>>)             -> bgm;
decode_channel(<<?TEMPO_CHANGE>>)    -> tempo_change;
decode_channel(<<?BGA>>)             -> bga;
decode_channel(<<?POOR_BITMAP>>)     -> poor_bitmap;
decode_channel(<<?BGA_LAYER>>)       -> bga_layer;
decode_channel(<<?EXT_BPM>>)         -> ext_bpm;
decode_channel(<<?P1_SCRATCH>>)      -> p1_scratch;
decode_channel(<<?P1_KEY_1>>)        -> p1_key_1;
decode_channel(<<?P1_KEY_2>>)        -> p1_key_2;
decode_channel(<<?P1_KEY_3>>)        -> p1_key_3;
decode_channel(<<?P1_KEY_4>>)        -> p1_key_4;
decode_channel(<<?P1_KEY_5>>)        -> p1_key_5;
decode_channel(<<?P1_KEY_6>>)        -> p1_key_6;
decode_channel(<<?P1_KEY_7>>)        -> p1_key_7;
decode_channel(<<?P1_SCRATCH_LONG>>) -> p1_scratch_long;
decode_channel(<<?P1_KEY_1_LONG>>)   -> p1_key_1_long;
decode_channel(<<?P1_KEY_2_LONG>>)   -> p1_key_2_long;
decode_channel(<<?P1_KEY_3_LONG>>)   -> p1_key_3_long;
decode_channel(<<?P1_KEY_4_LONG>>)   -> p1_key_4_long;
decode_channel(<<?P1_KEY_5_LONG>>)   -> p1_key_5_long;
decode_channel(<<?P1_KEY_6_LONG>>)   -> p1_key_6_long;
decode_channel(<<?P1_KEY_7_LONG>>)   -> p1_key_7_long;
decode_channel(<<?P2_SCRATCH>>)      -> p2_scratch;
decode_channel(<<?P2_KEY_1>>)        -> p2_key_1;
decode_channel(<<?P2_KEY_2>>)        -> p2_key_2;
decode_channel(<<?P2_KEY_3>>)        -> p2_key_3;
decode_channel(<<?P2_KEY_41>>)       -> p2_key_4;
decode_channel(<<?P2_KEY_5>>)        -> p2_key_5;
decode_channel(<<?P2_KEY_6>>)        -> p2_key_6;
decode_channel(<<?P2_KEY_7>>)        -> p2_key_7;
decode_channel(<<?P2_SCRATCH_LONG>>) -> p2_scratch_long;
decode_channel(<<?P2_KEY_1_LONG>>)   -> p2_key_1_long;
decode_channel(<<?P2_KEY_2_LONG>>)   -> p2_key_2_long;
decode_channel(<<?P2_KEY_3_LONG>>)   -> p2_key_3_long;
decode_channel(<<?P2_KEY_4_LONG>>)   -> p2_key_4_long;
decode_channel(<<?P2_KEY_5_LONG>>)   -> p2_key_5_long;
decode_channel(<<?P2_KEY_6_LONG>>)   -> p2_key_6_long;
decode_channel(<<?P2_KEY_7_LONG>>)   -> p2_key_7_long;
decode_channel(C)                    ->
    iidx_cli:warn("Unknown channel ~p", [C]),
    {unknown_channel, C}.


% A message length tells how many subdivisions are used to describe the channel in the current track.
% The message will describe every possible note in the tempo subdivision.
% This means that if the channel has just one note that needs a 1/16 arrangerment,
% the message will hold all possible 16 notes in order,
% 15 empty and 1 with the key sound id that needs to be played.
% If the channel has 1 note at the very beginning, there are no subdivisions and the only the played keysounf is listed.
% For example:
% 00030000
% 03
decode_message(Message) -> rec_decode_message(Message, []).

rec_decode_message(<<>>, Notes) ->
    lists:reverse(Notes);
rec_decode_message(<<Note:2/binary, Bin/binary>>, Notes) ->
    rec_decode_message(Bin, [Note | Notes]).
