-module(iidx_dot1).

-export([decode/1]).
-export([encode/1]).

-define(PINKY_CRUSH_DIRECTORIES, 14).
-define(CHART_TERMINATION, <<16#7FFFFFFF:32/integer-little, 0:32>>).

%--- IIDX Event Types ----------------------------------------------------------
% Source:
%       https://github.com/SaxxonPike/rhythm-game-formats/blob/master/iidx/1.md
%
% Visible Note
% Type: 00 (Player 1), 01 (Player 2)
% Parameter: Column number
% Value: If nonzero, defines the length of a freeze note in ticks
% If the value is zero, it's just a regular note.
-define(P1_NOTE, 16#00).
-define(P2_NOTE, 16#01).
% Sample Change
% Type: 02 (Player 1), 03 (Player 2)
% Parameter: Column number
% Value: Number of the sample to set the column to
% This is what determines which sound will play when a key is pressed,
% but does not actually make a sound by itself.
-define(P1_SAMPLE_CHANGE, 16#02).
-define(P2_SAMPLE_CHANGE, 16#03).
% Tempo Change
% Type: 04
% Parameter: Denominator
% Value: Numerator
% The BPM is a fraction determined by Numerator/Denominator.
% Songs can be found that have either 1 or 100 for the denominator,
% depending if a fractional BPM is used. A Parameter of 00 is NOT valid.
-define(TEMPO_CHANGE, 16#04).
% Meter Information
% Type: 05
% Parameter: Denominator
% Value: Numerator
% This tells the game how to count beats. Most songs are 4/4 meter,
% but songs such as Holic and Abstract would be in 7/8 part of the time.
% If I had to guess, it's used for the flashing graphic in the background of the play field for synchronizing with beats.
-define(METER_INFO, 16#05).
% End of Song
% Type: 06
% This just marks the end of the song.
% I think when this event is encountered,
% it signals to the game engine that it should fade out and go to grading.
-define(END_OF_SONG, 16#06).
% BGM Sound
% Type: 07
% Parameter: Stereo panning (01-0F, left to right, 08 is center)
% Value: Sound number to play
% This event just plays a sound in the background and is not visible on the field.
-define(BGM_SOUND, 16#07).
% Timing Window Information
% Type: 08
% Parameter: Which part of the window to define
% Value: Frame count
% The frame count is an 8-bit signed value
% and is counted in the number of frames ahead of the note a player presses.
% JUST GREAT is implicitly a window of 0 and 1.
% That means values below 0 are late, and values above 0 are early.
-define(TIMING_WINDOW_INFO, 16#08).
% Measure bar
% Type: 0C
% Parameter: Which player it is visible for
% This is just a visual element, rendered as a measure bar across the play field.
-define(MEASURE_BAR, 16#0C).
% Note Count Information
% Type: 10
% Parameter: Which player it is a count for
% Value: Number of playable notes
% This just indicates how many playable notes there are in the chart.
-define(NOTE_COUNT_INFO, 16#10).
%% IIDX Column mapping
-define(KEY_1,     16#00).
-define(KEY_2,     16#01).
-define(KEY_3,     16#02).
-define(KEY_4,     16#03).
-define(KEY_5,     16#04).
-define(KEY_6,     16#05).
-define(KEY_7,     16#06).
-define(SCRATCH,   16#07).

%--- API -----------------------------------------------------------------------

decode(Dot1FileBinary) ->
    Directories = decode_directory(Dot1FileBinary),
    [decode_chart(Dir, Dot1FileBinary) || Dir <- Directories].

encode(IIDX_Charts) ->
    EncodedCharts = [encode_chart(Chart) || Chart <- IIDX_Charts],
    EncodedDirectories = encode_directory(EncodedCharts),
    iolist_to_binary([EncodedDirectories, EncodedCharts]).

%--- Internals -----------------------------------------------------------------

% DECODING
decode_directory(<<FirstOffset:32/integer-little,
                  Length:32/integer-little,
                  Binary/binary>>) ->
    Directories = FirstOffset div 8,
    rec_decode_directory(2, Directories, [{FirstOffset, Length}], Binary).


rec_decode_directory(N, Max, ChartPos, _) when N > Max ->
    lists:reverse(ChartPos);
rec_decode_directory(N, Max, ChartPos, <<0:32/integer-little,
                                        0:32/integer-little,
                                        Binary/binary>>) -> % Skipping empty directories, should I track this or not ?
    iidx_cli:info("Skipping empty directory ~p", [N]),
    rec_decode_directory(N+1, Max, ChartPos, Binary);
rec_decode_directory(N, Max, ChartPos, <<Offset:32/integer-little,
                                        Length:32/integer-little,
                                        Binary/binary>>) ->
    rec_decode_directory(N+1, Max, [{Offset, Length} | ChartPos], Binary).

decode_chart({Pos, Len}, Binary) ->
    ChartBinary = binary:part(Binary, Pos, Len),
    rec_decode_chart(ChartBinary, []).

rec_decode_chart(<<>>, _) ->
    error(chart_termination);
rec_decode_chart(?CHART_TERMINATION, Events) ->
    lists:reverse(Events);
rec_decode_chart(<<Offset:32/integer-little, % in ticks
                  EventType:1/binary,
                  EventParam:8/little,
                  EventValue:16/integer-little,
                  Binary/binary>>, Events) ->
    {Type, Param, Value} = decode_event(EventType, EventParam, EventValue),
    Event = {Offset, Type, Param, Value},
    iidx_cli:info("Offset = ~p, Type = ~p, Param = ~p, Value = ~p",
        [Offset, Type, Param, Value]),
    rec_decode_chart(Binary, [Event | Events]).

decode_event(Type, Param, Value) ->
    DecodedType = decode_type(Type),
    DecodedParam = decode_param(DecodedType, Param),
    DecodedValue = decode_value(DecodedType, Value),
    {DecodedType, DecodedParam, DecodedValue}.

decode_param(p1_note, K) ->          decode_key(K);
decode_param(p2_note, K) ->          decode_key(K);
decode_param(p1_sample_change, K) -> decode_key(K);
decode_param(p2_sample_change, K) -> decode_key(K);
decode_param(note_count_info, 0)  -> p1;
decode_param(note_count_info, 1)  -> p2;
decode_param(_, P) -> P.

decode_value(_, Value) ->
    Value.

decode_type(<<?P1_NOTE>>)            -> p1_note;
decode_type(<<?P2_NOTE>>)            -> p2_note;
decode_type(<<?P1_SAMPLE_CHANGE>>)   -> p1_sample_change;
decode_type(<<?P2_SAMPLE_CHANGE>>)   -> p2_sample_change;
decode_type(<<?TEMPO_CHANGE>>)       -> tempo_change;
decode_type(<<?METER_INFO>>)         -> meter_info;
decode_type(<<?END_OF_SONG>>)        -> end_of_song;
decode_type(<<?BGM_SOUND>>)          -> bgm_sound;
decode_type(<<?TIMING_WINDOW_INFO>>) -> timing_window_info;
decode_type(<<?MEASURE_BAR>>)        -> measure_bar;
decode_type(<<?NOTE_COUNT_INFO>>)    -> note_count_info;
decode_type(T)                       -> error({bad_event_type, T}).

decode_key(?KEY_1)       -> key_1;
decode_key(?KEY_2)       -> key_2;
decode_key(?KEY_3)       -> key_3;
decode_key(?KEY_4)       -> key_4;
decode_key(?KEY_5)       -> key_5;
decode_key(?KEY_6)       -> key_6;
decode_key(?KEY_7)       -> key_7;
decode_key(?SCRATCH)     -> scratch;
decode_key(K)            -> error({bad_key_code, K}).

% ENCODING

encode_directory(Charts) when length(Charts) > ?PINKY_CRUSH_DIRECTORIES ->
    iidx_cli:abort("Too many charts, max is ~p", [?PINKY_CRUSH_DIRECTORIES]);
encode_directory(Charts) ->
    ChartSizes = [byte_size(C) || C <- Charts],
    StartOffset = directory_section_size(),
    rec_encode_directory(ChartSizes, 0, StartOffset, <<>>).

directory_section_size() ->
    ?PINKY_CRUSH_DIRECTORIES * 8.

rec_encode_directory([], ?PINKY_CRUSH_DIRECTORIES, _, Directories) ->
    Directories;
rec_encode_directory([], DirCount, SizeCount, Directories) ->
    EmptyDir = <<0:32/little, 0:32/little>>,
    rec_encode_directory([],
                         DirCount + 1,
                         SizeCount,
                         <<Directories/binary, EmptyDir/binary>>);
rec_encode_directory([ChartSize | OtherSizes], DirCount, ChartOffset, Directories) ->
    Directory = <<ChartOffset:32/little, ChartSize:32/little>>,
    rec_encode_directory(OtherSizes,
                         DirCount + 1,
                         ChartOffset + ChartSize,
                         <<Directories/binary, Directory/binary>>).

encode_chart(Chart) ->
    Events =
        lists:foldl(fun(Event, Binary) ->
                <<Binary/binary, (encode_event(Event))/binary>>
            end,
            <<>>,
            Chart),
    <<Events/binary, (?CHART_TERMINATION)/binary>>.

encode_event({Offset, Type, Param, Value}) ->
    EncodedType = encode_type(Type),
    EncodedParam = encode_param(Param, Type),
    EncodedValue = encode_value(Value, Type),
    <<Offset:32/integer-little,
      EncodedType:1/binary,
      EncodedParam:8/little,
      EncodedValue:16/integer-little>>.

encode_type(p1_note)            -> <<?P1_NOTE>>;
encode_type(p2_note)            -> <<?P2_NOTE>>;
encode_type(p1_sample_change)   -> <<?P1_SAMPLE_CHANGE>>;
encode_type(p2_sample_change)   -> <<?P2_SAMPLE_CHANGE>>;
encode_type(tempo_change)       -> <<?TEMPO_CHANGE>>;
encode_type(meter_info)         -> <<?METER_INFO>>;
encode_type(end_of_song)        -> <<?END_OF_SONG>>;
encode_type(bgm_sound)          -> <<?BGM_SOUND>>;
encode_type(timing_window_info) -> <<?TIMING_WINDOW_INFO>>;
encode_type(measure_bar)        -> <<?MEASURE_BAR>>;
encode_type(note_count_info)    -> <<?NOTE_COUNT_INFO>>;
encode_type(T)                  -> error({bad_event_type, T}).

encode_param(p1, note_count_info)   -> 0;
encode_param(p2, note_count_info)   -> 1;
encode_param(Key, p1_note)          -> encode_key(Key);
encode_param(Key, p2_note)          -> encode_key(Key);
encode_param(Key, p1_sample_change) -> encode_key(Key);
encode_param(Key, p2_sample_change) -> encode_key(Key);
encode_param(Param, _)              -> Param.

encode_value(Value, _) ->
    Value.

encode_key(key_1)   -> ?KEY_1;
encode_key(key_2)   -> ?KEY_2;
encode_key(key_3)   -> ?KEY_3;
encode_key(key_4)   -> ?KEY_4;
encode_key(key_5)   -> ?KEY_5;
encode_key(key_6)   -> ?KEY_6;
encode_key(key_7)   -> ?KEY_7;
encode_key(scratch) -> ?SCRATCH;
encode_key(K)       -> error({bad_key, K}).
