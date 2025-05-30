-module(iidx_mix).

-export([cli/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(music_artist_yomi, "contents/data/info/0/music_artist_yomi.xml").
-define(video_music_list, "contents/data/info/0/video_music_list.xml").
-define(music_title_yomi, "contents/data/info/0/music_title_yomi.xml").
-define(music_data, "contents/data/info/0/music_data.bin").

-define(shift_jis_prolog, "<?xml version=\"1.0\" encoding=\"Shift-JIS\"?>").

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
    help => "Extracts metadata from a BMS folder and adds it to the IIDX music_data.bin file",
    handler => fun mix/1
}.

mix(#{bms_folder := BMSfolder, iidx_folder := IIDXfolder, outdir := OutDir}) ->
    iidx_cli:assert_path_exists(IIDXfolder),
    iidx_cli:assert_path_exists(BMSfolder),

    IIDXData = read_iidx_files(IIDXfolder),

    BMSfiles = iidx_cli:find_files(BMSfolder, "*.bms"),
    iidx_cli:info("Found BMS files: ~p", [BMSfiles]),
    BMSBinaries = [iidx_cli:read_file(BMSfile) || BMSfile <- BMSfiles],
    BMSCharts = [iidx_bms:decode(BMSBinary) || BMSBinary <- BMSBinaries],
    BMSSongInfo = merge_song_metadata(BMSCharts),
    iidx_cli:info("~p", [BMSSongInfo]),
    NewIIDXData = add_song_to_catalog(32999, BMSSongInfo, IIDXData),
    write_iidx_files(NewIIDXData, OutDir),
    iidx_cli:success("Done!").

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
   MusicData = iidx_cli:read_file(filename:join(IIDXfolder, ?music_data)),
   IIDXData = read_shift_jis_files(IIDXfolder),
   IIDXData#{music_data => iidx_music_data:decode(MusicData)}.

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
    NewMusicData = MusicData#{data := MData ++ [Entry2]},
    NewData = edit_shift_jis_files(SongID, Title, Artist, IIDXData),
    NewData#{music_data => NewMusicData}.

write_iidx_files(IIDXData, OutDir) ->
    #{
        music_data := MusicData
    } = IIDXData,
    MusicDataBinary = iidx_music_data:encode(MusicData),
    MusicDataFilename = filename:join(OutDir, "music_data.bin"),
    iidx_cli:write_file(MusicDataFilename, MusicDataBinary),
    write_shift_jis_files(OutDir, IIDXData).

insert_null_bytes(Binary) ->
    << <<Char:8, 0:8>> || <<Char:8>> <= Binary >>.

-ifdef(ICONV).

read_shift_jis_files(IIDXfolder) ->
    MusicArtistYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_artist_yomi)),
    VideoMusicList = iidx_cli:read_file(filename:join(IIDXfolder, ?video_music_list)),
    MusicTitleYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_title_yomi)),
    MusicArtistYomiUtf8 = iidx_iconv:convert("shift-jis", "utf-8", MusicArtistYomi),
    VideoMusicListUtf8 = iidx_iconv:convert("shift-jis", "utf-8", VideoMusicList),
    MusicTitleYomiUtf8 = iidx_iconv:convert("shift-jis", "utf-8", MusicTitleYomi),
    {ArtistXML, _} = xmerl_scan:string(binary_to_list(MusicArtistYomiUtf8), [{encoding, "utf-8"}]),
    {VideoListXML, _} = xmerl_scan:string(binary_to_list(VideoMusicListUtf8), [{encoding, "utf-8"}]),
    {TitleXML, _} = xmerl_scan:string(binary_to_list(MusicTitleYomiUtf8), [{encoding, "utf-8"}]),
    #{
        music_artist_yomi => ArtistXML,
        video_music_list => VideoListXML,
        music_title_yomi => TitleXML
    }.

edit_shift_jis_files(SongID, Title, Artist, IIDXData) ->
    #{
        music_artist_yomi := ArtistXML,
        music_title_yomi := TitleXML,
        video_music_list := VideoListXML
    } = IIDXData,
    NewMusicTitleYomi = add_yomi_to_xml(SongID, Title, TitleXML),
    NewMusicArtistYomi = add_yomi_to_xml(SongID, Artist, ArtistXML),
    NewVideoMusicList = add_video_music_list_entry(SongID, Title, Artist, VideoListXML),
    IIDXData#{
        music_artist_yomi => NewMusicArtistYomi,
        video_music_list => NewVideoMusicList,
        music_title_yomi => NewMusicTitleYomi
    }.

add_yomi_to_xml(SongID, Title, XML) ->
    % Create new data element
    NewData = #xmlElement{
        name = data,
        attributes = [],
        content = [
            #xmlText{value = "\n    "},
            #xmlElement{
                name = index,
                attributes = [#xmlAttribute{name = '__type', value = "s32"}],
                content = [#xmlText{value = integer_to_list(SongID)}]
            },
            #xmlText{value = "\n    "},
            #xmlElement{
                name = yomi,
                attributes = [#xmlAttribute{name = '__type', value = "str"}],
                content = [#xmlText{value = Title}]
            },
            #xmlText{value = "\n  "}
        ]
    },
    #xmlElement{name = data_list, content = DataList} = XML,
    XML#xmlElement{
        content = [NewData | DataList]
    }.


add_video_music_list_entry(SongID, Title, Artist, XML) ->
    % Create new music element
    NewMusic = #xmlElement{
        name = music,
        attributes = [#xmlAttribute{name = id, value = integer_to_list(SongID)}],
        content = [
            #xmlText{value = "\n    "},
            #xmlElement{
                name = info,
                attributes = [],
                content = [
                    #xmlText{value = "\n      "},
                    #xmlElement{
                        name = title_name,
                        attributes = [],
                        content = [#xmlText{value = Title}]
                    },
                    #xmlText{value = "\n      "},
                    #xmlElement{
                        name = artist_name,
                        attributes = [],
                        content = [#xmlText{value = Artist}]
                    },
                    #xmlText{value = "\n      "},
                    #xmlElement{
                        name = play_video_flags,
                        attributes = [],
                        content = [#xmlText{value = "6"}]
                    },
                    #xmlText{value = "\n    "}
                ]
            },
            #xmlText{value = "\n  "}
        ]
    },
    #xmlElement{name = mdb, content = MusicList} = XML,
    XML#xmlElement{
        content = [#xmlText{value = "\n  "}, NewMusic | MusicList]
    }.

write_shift_jis_files( OutDir, IIDXData) ->
    #{
        music_artist_yomi := MusicArtistYomi,
        music_title_yomi := MusicTitleYomi,
        video_music_list := VideoMusicList
    } = IIDXData,
    MusicTitleYomiNewBinary = export_shift_jis_xml(MusicTitleYomi),
    MusicArtistYomiNewBinary = export_shift_jis_xml(MusicArtistYomi),
    VideoMusicListNewBinary = export_shift_jis_xml(VideoMusicList),
    MusicTitleYomiFilename = filename:join(OutDir, "music_title_yomi.xml"),
    MusicArtistYomiFilename = filename:join(OutDir, "music_artist_yomi.xml"),
    VideoMusicListFilename = filename:join(OutDir, "video_music_list.xml"),
    iidx_cli:write_file(MusicTitleYomiFilename, MusicTitleYomiNewBinary),
    iidx_cli:write_file(MusicArtistYomiFilename, MusicArtistYomiNewBinary),
    iidx_cli:write_file(VideoMusicListFilename, VideoMusicListNewBinary).

export_shift_jis_xml(XML) ->
    Binary = unicode:characters_to_binary(
        xmerl:export_simple([XML], xmerl_xml, [{prolog, ?shift_jis_prolog}])),
    iidx_iconv:convert("utf-8", "shift-jis", Binary).

-else.
% Empty functions for when iconv is not available

read_shift_jis_files(_) ->
    #{}.

edit_shift_jis_files(_, _, _, IIDXData) ->
    IIDXData.

write_shift_jis_files(_, _) ->
    ok.

-endif.
