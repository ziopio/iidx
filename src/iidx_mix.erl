-module(iidx_mix).

-export([cli/0]).

-define(music_artist_yomi, "contents/data/info/0/music_artist_yomi.xml").
-define(video_music_list, "contents/data/info/0/video_music_list.xml").
-define(music_title_yomi, "contents/data/info/0/music_title_yomi.xml").
-define(music_data, "contents/data/info/0/music_data.bin").

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
        }
    ],
    help => "Converts a BMS song into the .1 format a compatible with IIDX",
    handler => fun mix/1
}.

mix(#{bms_folder := BMSfolder, iidx_folder := IIDXfolder, outdir := OutDir}) ->
    iidx_cli:assert_path_exists(IIDXfolder),
    iidx_cli:assert_path_exists(BMSfolder),

    IIDXData = read_iidx_files(IIDXfolder),

    BMSfiles = iidx_cli:find_files(BMSfolder, "bms"),
    iidx_cli:info("Found BMS files: ~p", [BMSfiles]),
    BMSBinaries = [iidx_cli:read_file(BMSfile) || BMSfile <- BMSfiles],
    BMSCharts = [iidx_bms:decode(BMSBinary) || BMSBinary <- BMSBinaries],
    BMSSongInfo = merge_song_metadata(BMSCharts),
    iidx_cli:info("~p", [BMSSongInfo]),
    NewIIDXData = add_song_to_catalog(32999, BMSSongInfo, IIDXData),
    write_iidx_files(NewIIDXData, OutDir),
    ok.

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
              rank := Rank,
              subartist := SubArtistMap
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
                subartist => SubArtistMap,
                charts_specs => [ChartSpecific|ChartsSpecs]
            }
        end,
        #{charts_specs => []},
        BMSCharts).

read_iidx_files(IIDXfolder) ->
   %MusicArtistYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_artist_yomi)),
   %VideoMusicList = iidx_cli:read_file(filename:join(IIDXfolder, ?video_music_list)),
   %MusicTitleYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_title_yomi)),
    MusicData = iidx_cli:read_file(filename:join(IIDXfolder, ?music_data)),
    #{
        %music_artist_yomi => xmerl_scan:string(binary_to_list(MusicArtistYomi)),
        %video_music_list => xmerl_scan:string(binary_to_list(VideoMusicList)),
        %music_title_yomi => xmerl_scan:string(binary_to_list(MusicTitleYomi)),
        music_data => iidx_music_data:decode(MusicData)
    }.

add_song_to_catalog(SongID, BMSSongInfo, IIDXData) ->
    #{
        music_data := #{data := MData} = MusicData
    } = IIDXData,
    #{
        title := Title,
        artist := Artist,
        genre := Genre,
        %bpm := BPM,
        %subartist := SubArtistMap,
        charts_specs := ChartsSpecs
    } = BMSSongInfo,
    Entry = #{title => <<Title/binary, (binary:copy(<<0>>, 256 - byte_size(Title)))/binary>>,
              title_ascii => <<Title/binary, (binary:copy(<<0>>, 64 - byte_size(Title)))/binary>>,
              genre => <<Genre/binary, (binary:copy(<<0>>, 128 - byte_size(Genre)))/binary>>,
              artist => <<Artist/binary, (binary:copy(<<0>>, 256 - byte_size(Artist)))/binary>>,
              subtitle => binary:copy(<<0>>, 256),
              texture_title => 0,
              texture_artist => 0,
              texture_genre => 0,
              texture_load => 0,
              texture_list => 0,
              texture_subtitle => 0,
              font_idx => 4,
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
              bga_filename => <<51,50,48,56,49,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
              afp_flag => 0,
              afp_data => binary:copy(<<0>>, 320),
              unknown_section2 => <<0,0,0,0>>},
    Entry2 = lists:foldl(
        fun(#{difficulty := Difficulty,
              player := Player,
              playlevel := PlayLevel}, AccEntry) ->
            Key = case {Player, Difficulty} of
                        {single, <<"1">>} -> spb_level;
                        {single, <<"2">>} -> spn_level;
                        {single, <<"3">>} -> sph_level;
                        {single, <<"4">>} -> spa_level;
                        {single, <<"5">>} -> spl_level;
                        {couple, <<"1">>} -> dpb_level;
                        {couple, <<"2">>} -> dpn_level;
                        {couple, <<"3">>} -> dph_level;
                        {couple, <<"4">>} -> dpa_level;
                        {couple, <<"5">>} -> dpl_level
                  end,
            maps:put(Key, binary_to_integer(PlayLevel), AccEntry)
        end,
        Entry,
        ChartsSpecs),
    IIDXData#{music_data := MusicData#{data := MData ++ [Entry2]}}.

write_iidx_files(NewIIDXData, OutDir) ->
    #{
        music_data := MusicData
    } = NewIIDXData,
    iidx_cli:write_file(filename:join(OutDir, "music_data.bin"), iidx_music_data:encode(MusicData)).
