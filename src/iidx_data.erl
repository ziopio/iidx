%@doc This module is used to read and write the IIDX files.
-module(iidx_data).

-export([read_iidx_files/1]).
-export([edit_shift_jis_files/4]).
-export([write_iidx_files/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(data_path, <<"contents/data">>).
-define(info_path, <<"info/0">>).
-define(movie_path, <<"movie">>).
-define(sound_path(ID), <<"sound/", ID/binary>>).
-define(music_artist_yomi, <<"music_artist_yomi.xml">>).
-define(video_music_list, <<"video_music_list.xml">>).
-define(music_title_yomi, <<"music_title_yomi.xml">>).
-define(music_data, <<"music_data.bin">>).

-define(music_artist_yomi_path, filename:join([?data_path, ?info_path, ?music_artist_yomi])).
-define(video_music_list_path,  filename:join([?data_path, ?info_path, ?video_music_list])).
-define(music_title_yomi_path,  filename:join([?data_path, ?info_path, ?music_title_yomi])).
-define(music_data_path,  filename:join([?data_path, ?info_path, ?music_data])).

-define(shift_jis_prolog, "<?xml version=\"1.0\" encoding=\"Shift-JIS\"?>").

%--- API -----------------------------------------------------------------------

read_iidx_files(IIDXfolder) ->
    IIDXData = read_shift_jis_files(IIDXfolder),
    MusicData = iidx_cli:read_file(filename:join(IIDXfolder, ?music_data_path)),
    IIDXData#{?music_data => iidx_music_data:decode(MusicData)}.

write_iidx_files(IIDXData, OutDir) ->
    [write_iidx_file(Name, Binary, OutDir) || Name := Binary <- IIDXData].

write_iidx_file(?music_data = Name, MusicData, OutDir) ->
    SubDir = iidx_file_subdir(Name),
    Filename = filename:join([OutDir, SubDir, Name]),
    iidx_cli:write_file(Filename, iidx_music_data:encode(MusicData));
write_iidx_file(Name, XML, OutDir) when
  Name =:= ?music_artist_yomi orelse
  Name =:= ?music_title_yomi orelse
  Name =:= ?video_music_list ->
    SubDir = iidx_file_subdir(Name),
    Filename = filename:join([OutDir, SubDir, Name]),
    iidx_cli:write_file(Filename, export_shift_jis_xml(XML));
write_iidx_file(Name, Binary, OutDir) when is_binary(Binary) ->
    SubDir = iidx_file_subdir(Name),
    Filename = filename:join([OutDir, SubDir, Name]),
    iidx_cli:write_file(Filename, Binary).

iidx_file_subdir(?music_data) ->
    ?info_path;
iidx_file_subdir(?music_artist_yomi) ->
    ?info_path;
iidx_file_subdir(?video_music_list) ->
    ?info_path;
iidx_file_subdir(?music_title_yomi) ->
    ?info_path;
iidx_file_subdir(<<_:5/binary, ".mp4">>) ->
    ?movie_path;
iidx_file_subdir(<<SongID:5/binary, ".1">>) ->
    ?sound_path(SongID);
iidx_file_subdir(<<SongID:5/binary, "_pre.2dx">>) ->
    ?sound_path(SongID);
iidx_file_subdir(<<SongID:5/binary, ".s3p">>) ->
    ?sound_path(SongID).

-ifdef(ICONV).

read_shift_jis_files(IIDXfolder) ->
    MusicArtistYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_artist_yomi_path)),
    VideoMusicList = iidx_cli:read_file(filename:join(IIDXfolder, ?video_music_list_path)),
    MusicTitleYomi = iidx_cli:read_file(filename:join(IIDXfolder, ?music_title_yomi_path)),
    MusicArtistYomiUtf8 = iidx_iconv:convert("shift-jis", "utf-8", MusicArtistYomi),
    VideoMusicListUtf8 = iidx_iconv:convert("shift-jis", "utf-8", VideoMusicList),
    MusicTitleYomiUtf8 = iidx_iconv:convert("shift-jis", "utf-8", MusicTitleYomi),
    {ArtistXML, _} = xmerl_scan:string(binary_to_list(MusicArtistYomiUtf8), [{encoding, "utf-8"}]),
    {VideoListXML, _} = xmerl_scan:string(binary_to_list(VideoMusicListUtf8), [{encoding, "utf-8"}]),
    {TitleXML, _} = xmerl_scan:string(binary_to_list(MusicTitleYomiUtf8), [{encoding, "utf-8"}]),
    #{
        ?music_artist_yomi => ArtistXML,
        ?video_music_list => VideoListXML,
        ?music_title_yomi => TitleXML
    }.

edit_shift_jis_files(SongID, Title, Artist, IIDXData) ->
    #{
        ?music_artist_yomi := ArtistXML,
        ?music_title_yomi := TitleXML,
        ?video_music_list := VideoListXML
    } = IIDXData,
    NewMusicTitleYomi = add_yomi_to_xml(SongID, Title, TitleXML),
    NewMusicArtistYomi = add_yomi_to_xml(SongID, Artist, ArtistXML),
    NewVideoMusicList = add_video_music_list_entry(SongID, Title, Artist, VideoListXML),
    IIDXData#{
        ?music_artist_yomi => NewMusicArtistYomi,
        ?video_music_list => NewVideoMusicList,
        ?music_title_yomi => NewMusicTitleYomi
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

export_shift_jis_xml(_) ->
    error(iconv_disabled).

-endif.
