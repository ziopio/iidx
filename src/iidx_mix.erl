-module(iidx_mix).

-export([cli/0]).
-export([mix/3]).

%--- API -----------------------------------------------------------------------

cli() -> #{
    arguments => [
        #{
            name => bms_folder,
            type => binary,
            default => "."
        },
        #{
            name => iidx_folder,
            type => binary,
            default => "."
        },
        #{
            name => outdir,
            short => $o,
            long => "-outdir",
            type => binary,
            default => "."
        },
        #{
            name => song_id,
            short => $i,
            long => "-id",
            type => integer,
            default => 32097
        }
    ],
    help => "Extracts metadata from a BMS folder and adds it to the IIDX music_data.bin file",
    handler => fun mix_cmd/1
}.

mix_cmd(#{bms_folder := BMSfolder, iidx_folder := IIDXfolder, outdir := OutDir, song_id := SongID}) ->
    iidx_cli:assert_path_exists(IIDXfolder),
    iidx_cli:assert_path_exists(BMSfolder),
    IIDXData = iidx_data:read_iidx_files(IIDXfolder),
    NewIIDXData = mix(IIDXData, BMSfolder, SongID),
    iidx_data:write_iidx_files(NewIIDXData, OutDir),
    iidx_cli:success("Done!").

mix(IIDXData, BMSfolder, SongID) ->
    BMSfiles = iidx_cli:find_files(BMSfolder, "*.bms"),
    BMSBinaries = [iidx_cli:read_file(BMSfile) || BMSfile <- BMSfiles],
    BMSCharts = [iidx_bms:decode(BMSBinary) || BMSBinary <- BMSBinaries],
    BMSSongInfo = merge_song_metadata(BMSCharts),
    % iidx_cli:info("~p", [BMSSongInfo]),
    add_song_to_catalog(SongID, BMSSongInfo, IIDXData).

%--- Internals -----------------------------------------------------------------

% Merge all bms metadata from all bms files into a single song description
% that can later be used to write into the IIDX data files
merge_song_metadata(BMSCharts) ->
    lists:foldl(
        fun(#{header := Header}, #{charts_specs := ChartsSpecs} = SongInfo) ->
            #{title := Title,
              bpm := BPM,
              artist := Artist,
              difficulty := Difficulty,
              gauge_total := GaugeTotal,
              genre :=  Genre,
              long_note_type := LongNoteType,
              player := Player,
              playlevel := PlayLevel,
              rank := Rank
            } = Header,
            ChartSpecific = #{
                gauge_total => GaugeTotal,
                difficulty => Difficulty,
                long_note_type => LongNoteType,
                player => Player,
                playlevel => PlayLevel,
                rank => Rank
            },
            SongInfo#{
                title => Title,
                artist => Artist,
                genre => Genre,
                bpm => BPM,
                charts_specs => [ChartSpecific|ChartsSpecs]
            }
        end,
        #{charts_specs => []},
        BMSCharts).

add_song_to_catalog(SongID, BMSSongInfo, IIDXData) ->
    #{
        <<"music_data.bin">> := #{data := MData} = MusicData
    } = IIDXData,
    #{
        title := Title,
        artist := Artist,
        genre := Genre,
        %bpm := BPM,
        charts_specs := ChartsSpecs
    } = BMSSongInfo,
    TitleWithNulls = insert_null_bytes(Title),
    ArtistWithNulls = insert_null_bytes(Artist),
    GenreWithNulls = insert_null_bytes(Genre),
    Entry = #{title => <<TitleWithNulls/binary, (binary:copy(<<0>>, 256 - byte_size(TitleWithNulls)))/binary>>,
              title_ascii => <<Title/binary, (binary:copy(<<0>>, 64 - byte_size(Title)))/binary>>,
              genre => <<GenreWithNulls/binary, (binary:copy(<<0>>, 128 - byte_size(GenreWithNulls)))/binary>>,
              artist => <<ArtistWithNulls/binary, (binary:copy(<<0>>, 256 - byte_size(ArtistWithNulls)))/binary>>,
              subtitle => binary:copy(<<0>>, 256),
              texture_title => 0,
              texture_artist => 0,
              texture_genre => 0,
              texture_load => 0,
              texture_list => 0,
              texture_subtitle => 1,
              font_idx => 2,
              game_version => 32,
              other_folder => 1,
              bemani_folder => 0,
              beginner_rec_folder => 1,
              iidx_rec_folder => 0,
              bemani_rec_folder => 0,
              splittable_diff => 0,
              unk_unused => 0,
              spb_level => 0,
              spn_level => 0,
              sph_level => 0,
              spa_level => 0,
              spl_level => 0,
              dpb_level => 0,
              dpn_level => 0,
              dph_level => 0,
              dpa_level => 0,
              dpl_level => 0,
              unknown_section => binary:copy(<<0>>, 646),
              song_id => SongID,
              volume => 122,
              spb_ident => 48,
              spn_ident => 48,
              sph_ident => 48,
              spa_ident => 48,
              spl_ident => 48,
              dpb_ident => 48,
              dpn_ident => 48,
              dph_ident => 48,
              dpa_ident => 48,
              dpl_ident => 48,
              bga_delay => 0,
              bga_filename => <<(integer_to_binary(SongID))/binary, (binary:copy(<<0>>, 32 - 5))/binary>>,
              afp_flag => 0,
              afp_data => binary:copy(<<0>>, 320),
              unknown_section2 => <<0,0,0,0>>},
    Entry2 = lists:foldl(
        fun(#{difficulty := Difficulty,
              player := Player,
              playlevel := PlayLevel}, AccEntry) ->
            Key = case {Player, Difficulty} of
                        {single, beginner} -> spb_level;
                        {single, normal} -> spn_level;
                        {single, hyper} -> sph_level;
                        {single, another} -> spa_level;
                        {single, leggendaria} -> spl_level;
                        {double, beginner} -> dpb_level;
                        {double, normal} -> dpn_level;
                        {double, hyper} -> dph_level;
                        {double, another} -> dpa_level;
                        {double, leggendaria} -> dpl_level
                  end,
            maps:put(Key, PlayLevel, AccEntry)
        end,
        Entry,
        ChartsSpecs),
    NewMusicData = MusicData#{data := MData ++ [Entry2]},
    NewData = iidx_data:edit_shift_jis_files(SongID, Title, Artist, IIDXData),
    NewData#{<<"music_data.bin">> => NewMusicData}.

insert_null_bytes(Binary) ->
    << <<Char:8, 0:8>> || <<Char:8>> <= Binary >>.
