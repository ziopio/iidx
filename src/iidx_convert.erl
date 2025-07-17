-module(iidx_convert).

-export([cli/0]).
-export([convert/2]).

-include_lib("stdlib/include/assert.hrl").

%--- API -----------------------------------------------------------------------

cli() -> #{
    arguments => [
        #{
            name => bms_folder,
            type => binary,
            default => "."
        },
        #{
            name => iidx_id,
            short => $i,
            long => "-id",
            type => integer,
            default => 32097
        },
        #{
            name => outdir,
            short => $o,
            long => "-outdir",
            type => binary,
            default => "."
        }
    ],
    help => "Converts a BMS song into the .1 format a compatible with IIDX",
    handler => fun convert_cmd/1
}.

convert_cmd(#{bms_folder := BMSfolder, iidx_id := IIDXid, outdir := OutDir}) ->
    IIDXData = convert(BMSfolder, IIDXid),
    iidx_cli:success("Conversion completed!"),
    iidx_cli:info("Writing necessary file..."),
    iidx_data:write_iidx_files(IIDXData, OutDir),
    iidx_cli:success("Done!").

convert(BMSfolder, IIDXid) ->
    {SortedCharts, BMSAssets} = read_bms_folder(BMSfolder),
    BMSongData = create_sound_index(SortedCharts, BMSAssets),
    iidx_cli:success("BMS data has been loaded!"),
    iidx_cli:info("🔄🎵 Now converting song for IIDX 32 PinkyCrush! 🐷"),
    convert_bms_song_into_iidx(BMSongData, IIDXid).

%--- Internals -----------------------------------------------------------------

read_bms_folder(BMSfolder) ->
    iidx_cli:assert_path_exists(BMSfolder),
    BMSfiles = iidx_cli:find_files(BMSfolder, "*.bms"),
    iidx_cli:info("🧩🔍 Found BMS files: ~p", [BMSfiles]),
    BMSBinaries = [iidx_cli:read_file(BMSfile) || BMSfile <- BMSfiles],
    BMSCharts = [iidx_bms:decode(BMSBinary) || BMSBinary <- BMSBinaries],
    BMSrefs = merge_bms_file_references(BMSCharts),
    BMSAssets = read_all_bms_assets(BMSfolder, BMSrefs),
    {BMSCharts, BMSAssets}.

% Merge the assets of the BMS files into a single map
merge_bms_file_references(BMSCharts) ->
    lists:foldl(
        fun(Chart, Assets) ->
            BitMaps = get_all_images(Chart),
            WavFiles = mapz:deep_get([header, audio], Chart),
            BMSrefs = #{audio => WavFiles, bitmaps => BitMaps},
            mapz:deep_merge(BMSrefs, Assets)
        end,
        #{},
        BMSCharts).

get_all_images(Chart) ->
    BitMaps = mapz:deep_get([header, bitmaps], Chart, #{}),
    BackBitMapMap = case mapz:deep_get([header, back_bitmap], Chart, undefined) of
        undefined -> #{};
        BackBitMap -> #{back_bitmap => BackBitMap}
    end,
    maps:merge(BitMaps, BackBitMapMap).

% Read all the assets of the BMS files into a single map
read_all_bms_assets(BMSfolder, #{audio := WavFiles, bitmaps := BitMaps}) ->
    WavData = #{ID => read_audio(BMSfolder, Filename) || ID := Filename <- WavFiles},

    BitMapData = #{ID => iidx_cli:read_file(filename:join(BMSfolder, Filename))
                    || ID := Filename <- BitMaps},
    PreviewBin = read_audio(BMSfolder, <<"preview">>),
    Preview = #{
        data => PreviewBin,
        track => 0,
        attenuation => 1,
        loop => 0
    },
    #{audio => WavData, bitmaps => BitMapData, preview => Preview}.

read_audio(BMSfolder, <<"preview">> = Filename) ->
    case iidx_cli:find_files(BMSfolder, <<Filename/binary, ".*">>) of
        [File] ->
            convert_audiofile_to_wav(File);
        _ ->
            ok
    end,
    WavFilename = <<(filename:rootname(Filename))/binary, ".wav">>,
    iidx_cli:read_file(filename:join(BMSfolder, WavFilename));
read_audio(BMSfolder, Filename) ->
    case iidx_cli:find_files(BMSfolder, <<Filename/binary, ".*">>) of
        [File] ->
            convert_audiofile_to_asf(File);
        _ ->
            ok
    end,
    AsfFilename = <<(filename:rootname(Filename))/binary, ".asf">>,
    iidx_cli:read_file(filename:join(BMSfolder, AsfFilename)).

convert_audiofile_to_wav(FilePath) ->
    WavFilePath = filename:rootname(FilePath) ++ ".wav",
    iidx_cli:info("Converting ~s -> ~s", [FilePath, WavFilePath]),
    FFMPEG = os:find_executable("ffmpeg"),
    iidx_cli:exec(FFMPEG, ["-y", "-i", FilePath, "-t", "10", WavFilePath]).

convert_audiofile_to_asf(FilePath) ->
    AsfFilePath = filename:rootname(FilePath) ++ ".asf",
    iidx_cli:info("Converting ~s -> ~s", [FilePath, AsfFilePath]),
    FFMPEG = os:find_executable("ffmpeg"),
    iidx_cli:exec(FFMPEG, [
        "-i",
        FilePath,
        "-c:a", "wmav2",
        AsfFilePath]).

convert_bms_song_into_iidx({BMSCharts, Assets}, IIDXid) ->
    #{audio := WavMap,
      sound_index := WavIDs,
      bitmaps := _,
      preview := Preview} = Assets,
    iidx_cli:info("🆔🎯Using id ~p", [IIDXid]),
    IIDXCharts = [convert_bms_chart(C, WavIDs) || C <- BMSCharts],
    file:write_file("iidx_charts.debug.txt", io_lib:format("~p", [IIDXCharts])),
    Dot1Bin = iidx_dot1:encode(IIDXCharts),
    S3Vs = gen_s3vs(WavMap, WavIDs),
    S3PBin = iidx_s3p:encode(S3Vs),
    Pre2dxBin = iidx_2dx:encode(#{name => <<(integer_to_binary(IIDXid))/binary>>,
                                  files => #{"preview.wav" => Preview}}),
    Files = #{<<(integer_to_binary(IIDXid))/binary, ".1">> => Dot1Bin,
              <<(integer_to_binary(IIDXid))/binary, "_pre.2dx">> => Pre2dxBin,
              <<(integer_to_binary(IIDXid))/binary, ".s3p">> => S3PBin},
    ExtraFiles = generate_video(Assets, IIDXid),
    maps:merge(Files, ExtraFiles).

convert_bms_chart(Chart, WavIDs) ->
    Player = mapz:deep_get([header, player], Chart),
    Difficulty = mapz:deep_get([header, difficulty], Chart),
    InitialBPM = mapz:deep_get([header, bpm], Chart),
    InitialState = #{
        last_track => 0,
        bpm => InitialBPM,
        partial_long_notes => #{},
        messages => [],
        p1_notes => 0,
        p2_notes => 0,
        audio_index => WavIDs
    },
    BMSMessages = mapz:deep_get([data, messages], Chart),
    FinalState = lists:foldl(
        fun(Message, State) ->
            try convert_bms_message(Message, State)
            catch throw:{event_skip, E} ->
                iidx_cli:warn("Ignoring BMS message: ~p", [E]),
                State
            end
        end,
        InitialState,
        BMSMessages),
    #{messages := IIDXmessages,
      p1_notes := P1Notes,
      p2_notes := P2Notes,
      partial_long_notes := PartialLongNotes
    } = FinalState,
    ?assert(PartialLongNotes =:= #{}, "Partial long notes should be empty"),
    SongSetup =
        [
            {0, note_count_info, p1, P1Notes},
            {0, note_count_info, p2, P2Notes},
            {0, meter_info, 4, 4},
            {0, tempo_change, 100, InitialBPM * 100},
            % The timing window is completly arbitrary
            {0, timing_window_info, 0, 240},
            {0, timing_window_info, 1, 250},
            {0, timing_window_info, 2, 255},
            {0, timing_window_info, 3, 3},
            {0, timing_window_info, 4, 8},
            {0, timing_window_info, 5, 18},
            {0, bgm_sound, 0, 1},
            {0, measure_bar, 0, 0}
        ],
    SortedMessages = lists:sort(IIDXmessages),
    {LastTicks, _, _, _} = lists:last(SortedMessages),
    Footer = [
        {LastTicks + 1000, end_of_song, 0, 0},
        {LastTicks + 1000, end_of_song, 1, 0},
        {LastTicks + 1000, measure_bar, 0, 0},
        {LastTicks + 1000, measure_bar, 1, 0}
    ],
    {Player, Difficulty, SongSetup ++ SortedMessages ++ Footer}.

convert_bms_message({Track, BMSChannel, Notes}, State) ->
    #{bpm := BPM} = State,
    % A track is (240 / BPM) seconds long
    % A Tick is 1/1000 of a second
    TrackNumber = binary_to_integer(Track),
   % io:format("TrackNumber: ~p~n", [TrackNumber]),
    TrackTicks = round(TrackNumber * (240 / BPM) * 1000),
    State1 = add_measure_bar(TrackNumber, TrackTicks, State),
    IIDXEvent = bms_channel_to_iidx_event(BMSChannel),
   % io:format("IIDXEvent: ~p~n", [IIDXEvent]),
    add_notes(TrackTicks, IIDXEvent, Notes, State1).

add_measure_bar(TrackNumber, TrackTicks, #{last_track := LastTrack,
                                           messages := Messages} = State) ->
    case TrackNumber > LastTrack of
        false -> State;
        true ->
            % TODO add measurebar for player 2 if needed
            MeasureBar = {TrackTicks, measure_bar, 0, 0},
            State#{last_track => TrackNumber,
                   messages => [MeasureBar | Messages]}
    end.

add_notes(TrackTicks, {LengthType, PlayerSide, Key}, Notes, State) ->
    #{messages := Messages,
      audio_index := WavIndex
    } = State,
    Channel = {PlayerSide, Key},
    {NoteList, State1} = parse_beats(Notes, LengthType, Channel, State),
    NewMessages = lists:foldl(
        fun({TicksOffset, Sound, Duration}, Msgs) ->
            Ticks = TrackTicks + TicksOffset,
            SongIndex = maps:get(Sound, WavIndex),
            SampleChange = sample_change(Ticks, PlayerSide, Key, SongIndex),
            NoteStroke = {Ticks, PlayerSide, Key, Duration},
            [SampleChange, NoteStroke | Msgs]
        end,
        [],
        NoteList),
    State2 = count_notes(NewMessages, State1),
    State2#{messages => NewMessages ++ Messages};
add_notes(TrackTicks, bgm_sound, Notes, State) ->
    #{messages := Messages,
      audio_index := WavIndex
    } = State,
    NoteList = parse_single_beats(Notes, State),
    NewMessages = lists:foldl(
        fun({TicksOffset, Sound, _}, Msgs) ->
            Ticks = TrackTicks + TicksOffset,
            SongIndex = maps:get(Sound, WavIndex),
            [{Ticks, bgm_sound, 0, SongIndex} | Msgs]
        end,
        [],
        NoteList),
    State#{messages => NewMessages ++ Messages};
add_notes(_, E, _, State) ->
    iidx_cli:warn("Ignoring IIDX Event: ~p", [E]),
    State.

sample_change(Ticks, PlayerSide, Key, SongIndex) ->
    Event = case PlayerSide of
        p1_note -> p1_sample_change;
        p2_note -> p2_sample_change
    end,
    % For simplicity, the sample change is set 200 ms before the note
    {Ticks - 200, Event, Key, SongIndex}.

count_notes(NewMessages, #{p1_notes := P1_notes,
                           p2_notes := P2_notes} = State) ->
    {P1, P2} = lists:foldl(
        fun({_, p1_note, _, _}, {P1, P2}) -> {P1 + 1, P2};
           ({_, p2_note, _, _}, {P1, P2}) -> {P1, P2 + 1};
           (_, Acc) -> Acc
        end,
        {P1_notes, P2_notes},
        NewMessages),
    State#{p1_notes => P1, p2_notes => P2}.

bms_channel_to_iidx_event(bgm)             -> bgm_sound;
bms_channel_to_iidx_event(tempo_change)    -> tempo_change;
bms_channel_to_iidx_event(p1_scratch)      -> {short, p1_note, scratch};
bms_channel_to_iidx_event(p1_key_1)        -> {short, p1_note, key_1};
bms_channel_to_iidx_event(p1_key_2)        -> {short, p1_note, key_2};
bms_channel_to_iidx_event(p1_key_3)        -> {short, p1_note, key_3};
bms_channel_to_iidx_event(p1_key_4)        -> {short, p1_note, key_4};
bms_channel_to_iidx_event(p1_key_5)        -> {short, p1_note, key_5};
bms_channel_to_iidx_event(p1_key_6)        -> {short, p1_note, key_6};
bms_channel_to_iidx_event(p1_key_7)        -> {short, p1_note, key_7};
bms_channel_to_iidx_event(p1_scratch_long) -> {long, p1_note, scratch};
bms_channel_to_iidx_event(p1_key_1_long)   -> {long, p1_note, key_1};
bms_channel_to_iidx_event(p1_key_2_long)   -> {long, p1_note, key_2};
bms_channel_to_iidx_event(p1_key_3_long)   -> {long, p1_note, key_3};
bms_channel_to_iidx_event(p1_key_4_long)   -> {long, p1_note, key_4};
bms_channel_to_iidx_event(p1_key_5_long)   -> {long, p1_note, key_5};
bms_channel_to_iidx_event(p1_key_6_long)   -> {long, p1_note, key_6};
bms_channel_to_iidx_event(p1_key_7_long)   -> {long, p1_note, key_7};
bms_channel_to_iidx_event(p2_scratch)      -> {short, p2_note, scratch};
bms_channel_to_iidx_event(p2_key_1)        -> {short, p2_note, key_1};
bms_channel_to_iidx_event(p2_key_2)        -> {short, p2_note, key_2};
bms_channel_to_iidx_event(p2_key_3)        -> {short, p2_note, key_3};
bms_channel_to_iidx_event(p2_key_4)        -> {short, p2_note, key_4};
bms_channel_to_iidx_event(p2_key_5)        -> {short, p2_note, key_5};
bms_channel_to_iidx_event(p2_key_6)        -> {short, p2_note, key_6};
bms_channel_to_iidx_event(p2_key_7)        -> {short, p2_note, key_7};
bms_channel_to_iidx_event(p2_scratch_long) -> {long, p2_note, scratch};
bms_channel_to_iidx_event(p2_key_1_long)   -> {long, p2_note, key_1};
bms_channel_to_iidx_event(p2_key_2_long)   -> {long, p2_note, key_2};
bms_channel_to_iidx_event(p2_key_3_long)   -> {long, p2_note, key_3};
bms_channel_to_iidx_event(p2_key_4_long)   -> {long, p2_note, key_4};
bms_channel_to_iidx_event(p2_key_5_long)   -> {long, p2_note, key_5};
bms_channel_to_iidx_event(p2_key_6_long)   -> {long, p2_note, key_6};
bms_channel_to_iidx_event(p2_key_7_long)   -> {long, p2_note, key_7};
bms_channel_to_iidx_event(E)               -> throw({event_skip, E}).


parse_beats(ChannelNotes, short, _, State) ->
    NoteList = parse_single_beats(ChannelNotes, State),
    {NoteList, State};
parse_beats(ChannelNotes, long, Channel, State) ->
    parse_long_beats(ChannelNotes, Channel, State).

parse_single_beats(ChannelNotes, #{bpm := BPM}) ->
    TrackResolution = length(ChannelNotes),
    TrackTicks = 240 / BPM * 1000,
    NoteStride = round(TrackTicks / TrackResolution),
    {_, Notes} = lists:foldl(fun
        (<<"00">>, {Offset, Notes}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, Notes};
        (Sound, {Offset, Acc}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, [{Offset, Sound, 0} | Acc]}
        end,
        {0, []},
        ChannelNotes),
    Notes.

parse_long_beats(ChannelNotes, Channel, State) ->
    #{bpm := BPM, partial_long_notes := PartialLongNotes} = State,
    TrackResolution = length(ChannelNotes),
   % io:format("TrackResolution: ~p~n", [TrackResolution]),
    TrackTicks = 240 / BPM * 1000,
    NoteStride = round(TrackTicks / TrackResolution),
    % Tracks each long note in the channel to measure its length.
    % Each note is present 2 times to signal start and end.
    % Once the note ends we store it together with its length.
    % Long notes could extend over multiple tracks.
    % This function needs to return the partial note state.
    %
    % Accumulator is {Offset, CurrentSound, CurrentLenght, NotesAcc}
    {PreviousKeySound, PreviousLenght} =
                        maps:get(Channel, PartialLongNotes, {undefined, 0}),
    StartAcc = {0, PreviousKeySound, PreviousLenght, []},
    {_, LastKeySound, LeftLenght, Notes} = lists:foldl(fun
        (<<"00">>, {Offset, undefined, NoteLenght, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, undefined, NoteLenght, NotesAcc};
        (<<"00">>, {Offset, LastKeySound, NoteLenght, NotesAcc}) ->
            NewNoteLenght = NoteLenght + NoteStride,
            NewOffset = Offset + NoteStride,
            {NewOffset, LastKeySound, NewNoteLenght, NotesAcc};
        (NewKeySound, {Offset, undefined, 0, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, NewKeySound, NoteStride, NotesAcc};
        (KeySound, {Offset, KeySound, NoteLenght, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            LongNote = {Offset, KeySound, NoteLenght},
            {NewOffset, undefined, 0, [LongNote | NotesAcc]}
        end,
        StartAcc,
        ChannelNotes),
    % If the last key sound is undefined, it means that the long note has ended.
    % Remove it from the partial long notes.
    % Otherwise, update the partial long notes, with the last key sound and the left lenght.
    NewPartialLongNotes = case LastKeySound of
        undefined -> maps:remove(Channel, PartialLongNotes);
        _ -> maps:put(Channel, {LastKeySound, LeftLenght}, PartialLongNotes)
    end,
    NewState = State#{partial_long_notes => NewPartialLongNotes},
    {Notes, NewState}.

gen_s3vs(WavMap, WavIndex) ->
    IndexToBin = #{V => maps:get(K, WavMap) || K := V <- WavIndex},
    SortedKeySounds = [Bin || {_, Bin} <- lists:sort(maps:to_list(IndexToBin))],
    % The unknown field could be anything, maybe is an unique ID
    [#{unk => crypto:strong_rand_bytes(4), data => S} || S <- SortedKeySounds].

generate_video(#{bitmaps := BitMapMap}, _) when BitMapMap =:= #{} ->
    iidx_cli:warn("No bitmap found, skipping video generation!"),
    #{};
generate_video(#{bitmaps := BitMapMap}, IIDXid) ->
    [FirstBitMap|_] = maps:values(BitMapMap),
    TempDir = iidx_cli:get_temp_dir(),
    TempImage = filename:join(TempDir, <<"temp_", (integer_to_binary(IIDXid))/binary, ".bmp">>),
    TempVideo = filename:join(TempDir, <<"temp_", (integer_to_binary(IIDXid))/binary, ".mp4">>),
    ok = file:write_file(TempImage, FirstBitMap),
    FFMPEG = os:find_executable("ffmpeg"),
    Args = [
        "-loop", "1",
        "-i", TempImage,
        "-c:v", "libx264", "-t", "1", "-pix_fmt", "yuv420p",
        TempVideo
    ],
    iidx_cli:exec(FFMPEG, Args),
    MP4Video = iidx_cli:read_file(TempVideo),
    file:delete(TempVideo),
    file:delete(TempImage),
    #{<<(integer_to_binary(IIDXid))/binary, ".mp4">> => MP4Video}.

create_sound_index(BMSCharts, #{audio := WavMap} = Assets) ->
    ExpectedKeysounds = parse_all_keysounds(BMSCharts),
    SilentWavBinary = generate_silent_keysound(),
    % If the song is missing keysounds, generate them.
    CoherentWavData = lists:foldl(
        fun(KeySound, Acc) ->
            case maps:get(KeySound, WavMap, undefined) of
                undefined ->
                    iidx_cli:warn("Missing audio for keysound: ~p", [KeySound]),
                    iidx_cli:warn("Using silent keysound as placeholder!"),
                    maps:put(KeySound, SilentWavBinary, Acc);
                _ -> Acc
            end
        end, WavMap, ExpectedKeysounds),
    % Now every used keysound present in the BMS chart has its audio file.
    % Create a map of the indices to the keysounds IDs.
    IndexedIDs = lists:zip(maps:keys(CoherentWavData),
                           lists:seq(1, map_size(CoherentWavData))),
    WavIDs = #{K => I || {K, I} <- IndexedIDs},
    {BMSCharts, Assets#{audio => CoherentWavData, sound_index => WavIDs}}.

parse_all_keysounds(BMSCharts) ->
    Notes = lists:foldl(
        fun(Chart, Acc) ->
            Events = mapz:deep_get([data, messages], Chart),
            Notes = [ [C || C <- Content, C =/= <<"00">>]
                    || {_Track, _Type, Content} <- Events, is_list(Content)],
            [Notes|Acc]
        end,
        [],
        BMSCharts),
    sets:to_list(sets:from_list(lists:flatten(Notes))).

generate_silent_keysound() ->
    SilentKeysoundFile = filename:join(iidx_cli:get_temp_dir(), "empty.asf"),
    % Generate 100ms of silence
    FFMPEG = os:find_executable("ffmpeg"),
    iidx_cli:exec(FFMPEG, [
        "-f", "lavfi",
        "-i", "anullsrc=channel_layout=stereo:sample_rate=44100",
        "-t", "0.1",
        "-c:a", "wmav2",
        SilentKeysoundFile
        ]),
    Binary = iidx_cli:read_file(SilentKeysoundFile),
    file:delete(SilentKeysoundFile),
    Binary.
