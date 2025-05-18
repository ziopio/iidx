-module(iidx_convert).

-export([cli/0]).

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
            long => "-identifier",
            type => binary,
            default => <<"32999">>
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
    handler => fun convert/1
}.

convert(#{bms_folder := BMSfolder, iidx_id := IIDXid, outdir := OutDir}) ->
    BMSongData = read_bms_folder(BMSfolder),
    iidx_cli:success("BMS data has been loaded!"),
    iidx_cli:info("Now converting song for IIDX 32 PinkyCrush!"),
    IIDXFiles = convert_bms_song_into_iidx(BMSongData, IIDXid),
    iidx_cli:success("Conversion completed!"),
    iidx_cli:info("Writing necessary file..."),
    [iidx_cli:write_file(filename:join(OutDir, Name), Binary)
     || Name := Binary <- IIDXFiles],
    iidx_cli:success("Done!"),
    ok.

%--- Internals -----------------------------------------------------------------


read_bms_folder(BMSfolder) ->
    iidx_cli:assert_path_exists(BMSfolder),
    BMSfiles = iidx_cli:find_files(BMSfolder, "*.bms"),
    iidx_cli:info("Found BMS files: ~p", [BMSfiles]),
    BMSBinaries = [iidx_cli:read_file(BMSfile) || BMSfile <- BMSfiles],
    BMSCharts = [iidx_bms:decode(BMSBinary) || BMSBinary <- BMSBinaries],
    BMSrefs = merge_bms_file_references(BMSCharts),
    BMSAssets = read_all_bms_assets(BMSfolder, BMSrefs),
    SortedCharts = lists:sort(fun cmp_play_level/2, BMSCharts),
    {SortedCharts, BMSAssets}.

cmp_play_level(Chart1, Chart2) ->
    Level1 = mapz:deep_get([header, playlevel], Chart1),
    Level2 = mapz:deep_get([header, playlevel], Chart2),
    binary_to_integer(Level1) =< binary_to_integer(Level2).

% Merge the assets of the BMS files into a single map
merge_bms_file_references(BMSCharts) ->
    lists:foldl(
        fun(Chart, Assets) ->
            BitMaps = mapz:deep_get([header, bitmap], Chart),
            WavFiles = mapz:deep_get([header, audio], Chart),
            BMSrefs = #{audio => WavFiles, bitmap => BitMaps},
            mapz:deep_merge(BMSrefs, Assets)
        end,
        #{},
        BMSCharts).

% Read all the assets of the BMS files into a single map
read_all_bms_assets(BMSfolder, #{audio := WavFiles, bitmap := BitMaps}) ->
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
    #{audio => WavData, bitmap => BitMapData, preview => Preview}.

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
    iidx_cli:exec(FFMPEG, ["-i", FilePath, WavFilePath]).

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
      bitmap := _,
      preview := Preview} = Assets,
    iidx_cli:info("Using id ~p", [IIDXid]),
    IndexedIDs = lists:zip(maps:keys(WavMap), lists:seq(1, map_size(WavMap))),
    WavIDs = #{K => I || {K, I} <- IndexedIDs},
    IIDXCharts = [convert_bms_chart(C, WavIDs) || C <- BMSCharts],
    file:write_file("iidx_charts.debug.txt", io_lib:format("~p", [IIDXCharts])),
    Dot1Bin = iidx_dot1:encode(IIDXCharts),
    S3Vs = gen_s3vs(WavMap, WavIDs),
    S3PBin = iidx_s3p:encode(S3Vs),
    Pre2dxBin = iidx_2dx:encode(#{name => <<IIDXid/binary>>,
                                  files => #{"preview.wav" => Preview}}),
    #{<<IIDXid/binary, ".1">> => Dot1Bin,
      <<IIDXid/binary, "_pre.2dx">> => Pre2dxBin,
      <<IIDXid/binary, ".s3p">> => S3PBin}.

convert_bms_chart(Chart, WavIDs) ->
    InitialBPM = mapz:deep_get([header, bpm], Chart),
    InitialState = #{
        last_track => 0,
        bpm => InitialBPM,
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
      p2_notes := P2Notes
    } = FinalState,
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
    SongSetup ++ SortedMessages ++ Footer.

convert_bms_message({Track, BMSChannel, Notes}, State) ->
    #{bpm := BPM} = State,
     % A track is (240 / BPM) seconds long
     % A Tick is 1/1000 of a second
    TrackNumber = binary_to_integer(Track),
    TrackTicks = round(TrackNumber * (240 / BPM) * 1000),
    State1 = add_measure_bar(TrackNumber, TrackTicks, State),
    IIDXEvent = bms_channel_to_iidx_event(BMSChannel),
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

add_notes(TrackTicks, {LengthType, NoteSide, Key}, Notes, State) ->
    #{
      bpm := BPM,
      messages := Messages,
      audio_index := WavIndex
    } = State,
    NoteList = case LengthType of
        short -> parse_single_beats(Notes, BPM);
        long -> parse_long_beats(Notes, BPM)
    end,
    NewMessages = lists:foldl(
        fun({TicksOffset, Sound, Duration}, Msgs) ->
            Ticks = TrackTicks + TicksOffset,
            SongIndex = maps:get(Sound, WavIndex),
            SampleChange = sample_change(Ticks, NoteSide, Key, SongIndex),
            NoteStroke = {Ticks, NoteSide, Key, Duration},
            [SampleChange, NoteStroke | Msgs]
        end,
        [],
        NoteList),
    State1 = count_notes(NewMessages, State),
    State1#{messages => NewMessages ++ Messages};
add_notes(TrackTicks, bgm_sound, Notes, State) ->
    #{
      bpm := BPM,
      messages := Messages,
      audio_index := WavIndex
    } = State,
    NoteList = parse_single_beats(Notes, BPM),
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

sample_change(Ticks, NoteSide, Key, SongIndex) ->
    Event = case NoteSide of
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

parse_single_beats(ChannelNotes, BPM) ->
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

parse_long_beats(ChannelNotes, BPM) ->
    TrackResolution = length(ChannelNotes),
    TrackTicks = 240 / BPM * 1000,
    NoteStride = round(TrackTicks / TrackResolution),
    {undefined, Notes} = lists:foldl(fun
        (<<"00">>, {Offset, undefined, NoteLenght, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, undefined, NoteLenght, NotesAcc};
        (<<"00">>, {Offset, LastSound, NoteLenght, NotesAcc}) ->
            NewNoteLenght = NoteLenght + NoteStride,
            NewOffset = Offset + NoteStride,
            {NewOffset, LastSound, NewNoteLenght, NotesAcc};
        (NewSound, {Offset, undefined, 0, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            {NewOffset, NewSound, NoteStride, NotesAcc};
        (Sound, {Offset, Sound, NoteLenght, NotesAcc}) ->
            NewOffset = Offset + NoteStride,
            LongNote = {Offset, Sound, NoteLenght + NoteStride},
            {NewOffset, undefined, 0, [LongNote | NotesAcc]}
        end,
        {0, undefined, 0, []},
        ChannelNotes),
    Notes.

gen_s3vs(WavMap, WavIndex) ->
    IndexToBin = #{V => maps:get(K, WavMap) || K := V <- WavIndex},
    SortedKeySounds = [Bin || {_, Bin} <- lists:sort(maps:to_list(IndexToBin))],
    % The unknown field could be anything, maybe is an unique ID
    [#{unk => crypto:strong_rand_bytes(4), data => S} || S <- SortedKeySounds].
