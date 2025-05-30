-module(iidx_music_data).

-include_lib("stdlib/include/assert.hrl").

-export([decode/1]).
-export([encode/1]).

-define(MAX_ENTRIES_PER_STYLE, 1000).
-define(CURRENT_STYLE_ENTRIES(X), X * ?MAX_ENTRIES_PER_STYLE).
-define(MAX_ENTRIES(X), ?CURRENT_STYLE_ENTRIES(X) + ?MAX_ENTRIES_PER_STYLE).

%--- API -----------------------------------------------------------------------

decode(MusicDataFileBinary) ->
    {Header, DataBin} = decode_header(MusicDataFileBinary),
    Data = decode_data(DataBin),
    #{header => Header, data => Data}.

encode(#{header := Header, data := Data}) ->
    SongIDs = [SongID || #{song_id := SongID} <- Data],
    HeaderBin = encode_header(Header, SongIDs),
    DataBin = encode_data(Data),
    <<HeaderBin/binary, DataBin/binary>>.

%--- Internals -----------------------------------------------------------------

% DECODING

decode_header(<<"IIDX", Version:32/little,
                _AvailableEntries:16/integer-little,
                0:16/integer-little,
                TotalEntries:32/integer-little, Binary/binary>>) ->
    % Ensure is from pinky crush and not from a different style
    ?assert(Version == 32),
    ?assert(?MAX_ENTRIES(Version) == TotalEntries),
    % There is no need to parse the song index.
    % The song index is a list of indices pointing to the music array.
    % This information is redundant and relevant only when encoding for IIDX.
    SongIndexSize = TotalEntries * 4,
    <<_SongIndex:SongIndexSize/binary, Data/binary>> = Binary,
    Header = #{
        version => Version,
        total_entries => TotalEntries
    },
    {Header, Data}.

decode_data(DataBin) ->
    rec_decode_data(DataBin, []).

rec_decode_data(<<>>, Data) ->
    lists:reverse(Data);
rec_decode_data(<<Title:256/binary,
                  TitleASCII:64/binary,
                  Genre:128/binary,
                  Artist:256/binary,
                  Subtitle:256/binary,
                  Texture_title:32/little,
                  Texture_artist:32/little,
                  Texture_genre:32/little,
                  Texture_load:32/little,
                  Texture_list:32/little,
                  Texture_subtitle:32/little,
                  FontIDX:32/little,
                  GameVerion:16/little,
                  Other_folder:16/little,
                  Bemani_folder:16/little,
                  Beginner_rec_folder:16/little,
                  Iidx_rec_folder:16/little,
                  Bemani_rec_folder:16/little,
                  Splittable_diff:16/little,
                  Unk_unused:16/little,
                  SPB_level:8/little,
                  SPN_level:8/little,
                  SPH_level:8/little,
                  SPA_level:8/little,
                  SPL_level:8/little,
                  DPB_level:8/little,
                  DPN_level:8/little,
                  DPH_level:8/little,
                  DPA_level:8/little,
                  DPL_level:8/little,
                  _UnknownSection:646/binary,
                  SongID:32/little,
                  Volume:32/little,
                  SPB_ident:8/little,
                  SPN_ident:8/little,
                  SPH_ident:8/little,
                  SPA_ident:8/little,
                  SPL_ident:8/little,
                  DPB_ident:8/little,
                  DPN_ident:8/little,
                  DPH_ident:8/little,
                  DPA_ident:8/little,
                  DPL_ident:8/little,
                  BGA_delay:16/little,
                  BGA_filename:32/binary,
                  AFP_flag:32/little,
                  AFP_data:320/binary,
                  _UnknownSection2:4/binary,
                  Binary/binary>>, Data) ->
    Entry = #{
        title => Title,
        title_ascii => TitleASCII,
        genre => Genre,
        artist => Artist,
        subtitle => Subtitle,
        texture_title => Texture_title,
        texture_artist => Texture_artist,
        texture_genre => Texture_genre,
        texture_load => Texture_load,
        texture_list => Texture_list,
        texture_subtitle => Texture_subtitle,
        font_idx => FontIDX,
        game_version => GameVerion,
        other_folder => Other_folder,
        bemani_folder => Bemani_folder,
        beginner_rec_folder => Beginner_rec_folder,
        iidx_rec_folder => Iidx_rec_folder,
        bemani_rec_folder => Bemani_rec_folder,
        splittable_diff => Splittable_diff,
        unk_unused => Unk_unused,
        spb_level => SPB_level,
        spn_level => SPN_level,
        sph_level => SPH_level,
        spa_level => SPA_level,
        spl_level => SPL_level,
        dpb_level => DPB_level,
        dpn_level => DPN_level,
        dph_level => DPH_level,
        dpa_level => DPA_level,
        dpl_level => DPL_level,
        unknown_section => _UnknownSection,
        song_id => SongID,
        volume => Volume,
        spb_ident => SPB_ident,
        spn_ident => SPN_ident,
        sph_ident => SPH_ident,
        spa_ident => SPA_ident,
        spl_ident => SPL_ident,
        dpb_ident => DPB_ident,
        dpn_ident => DPN_ident,
        dph_ident => DPH_ident,
        dpa_ident => DPA_ident,
        dpl_ident => DPL_ident,
        bga_delay => BGA_delay,
        bga_filename => BGA_filename,
        afp_flag => AFP_flag,
        afp_data => AFP_data,
        unknown_section2 => _UnknownSection2
    },
    rec_decode_data(Binary, [Entry|Data]).


% ENCODING

encode_header(#{version := Version,
                total_entries := TotalEntries}, SongIDs) ->
    ?assert(?MAX_ENTRIES(Version) == TotalEntries),
    SongIDSection = encode_song_index(Version, SongIDs, TotalEntries),
    <<"IIDX",
      Version:32/little,
      (length(SongIDs)):32/integer-little,
      TotalEntries:32/integer-little,
      SongIDSection/binary>>.

%% The IIDX song index is a list of indices pointing to the music array.
%% The song index contains an index for every possible song ID ever used in IIDX.
%% This means it is as long as the maximum amount of SongIDs!!!
%% If the index at a certain position is valid, it means that the song ID is present in the file.
%% The index value is the position in the array of music data, where that song ID is described.
%% If a SongID is not present in the file,
%% the index will be 0 if the song is from the latest style,
%% or 0xFFFFFFFF if the song ID is older then the current style.
encode_song_index(Version, SongIDs, TotalEntries) ->
    {_, Bin} = lists:foldl(fun (ID, {CurrentSong, Binary}) ->
            case lists:member(ID, SongIDs) of
                true ->
                    {CurrentSong + 1, <<Binary/binary, CurrentSong:32/little>>};
                false ->
                    case ID >= ?CURRENT_STYLE_ENTRIES(Version) of
                        true -> {CurrentSong, <<Binary/binary, 0:32/little>>};
                        false -> {CurrentSong, <<Binary/binary, 16#FFFFFFFF:32/little>>}
                    end
            end
        end,
        {0, <<>>},
        lists:seq(0, TotalEntries - 1)),
    Bin.

encode_data(Data) ->
    lists:foldl(fun encode_entry/2, <<>>, Data).

encode_entry(#{title := Title,
               title_ascii := TitleASCII,
               genre := Genre,
               artist := Artist,
               subtitle := Subtitle,
               texture_title := Texture_title,
               texture_artist := Texture_artist,
               texture_genre := Texture_genre,
               texture_load := Texture_load,
               texture_list := Texture_list,
               texture_subtitle := Texture_subtitle,
               font_idx := FontIDX,
               game_version := GameVerion,
               other_folder := Other_folder,
               bemani_folder := Bemani_folder,
               beginner_rec_folder := Beginner_rec_folder,
               iidx_rec_folder := Iidx_rec_folder,
               bemani_rec_folder := Bemani_rec_folder,
               splittable_diff := Splittable_diff,
               unk_unused := Unk_unused,
               spb_level := SPB_level,
               spn_level := SPN_level,
               sph_level := SPH_level,
               spa_level := SPA_level,
               spl_level := SPL_level,
               dpb_level := DPB_level,
               dpn_level := DPN_level,
               dph_level := DPH_level,
               dpa_level := DPA_level,
               dpl_level := DPL_level,
               unknown_section := UnknownSection,
               song_id := SongID,
               volume := Volume,
               spb_ident := SPB_ident,
               spn_ident := SPN_ident,
               sph_ident := SPH_ident,
               spa_ident := SPA_ident,
               spl_ident := SPL_ident,
               dpb_ident := DPB_ident,
               dpn_ident := DPN_ident,
               dph_ident := DPH_ident,
               dpa_ident := DPA_ident,
               dpl_ident := DPL_ident,
               bga_delay := BGA_delay,
               bga_filename := BGA_filename,
               afp_flag := AFP_flag,
               afp_data := AFP_data,
               unknown_section2 := UnknownSection2}, Acc) ->
    <<Acc/binary,
      Title:16#100/binary,
      TitleASCII:16#40/binary,
      Genre:16#80/binary,
      Artist:16#100/binary,
      Subtitle:16#100/binary,
      Texture_title:32/little,
      Texture_artist:32/little,
      Texture_genre:32/little,
      Texture_load:32/little,
      Texture_list:32/little,
      Texture_subtitle:32/little,
      FontIDX:32/little,
      GameVerion:16/little,
      Other_folder:16/little,
      Bemani_folder:16/little,
      Beginner_rec_folder:16/little,
      Iidx_rec_folder:16/little,
      Bemani_rec_folder:16/little,
      Splittable_diff:16/little,
      Unk_unused:16/little,
      SPB_level:8/little,
      SPN_level:8/little,
      SPH_level:8/little,
      SPA_level:8/little,
      SPL_level:8/little,
      DPB_level:8/little,
      DPN_level:8/little,
      DPH_level:8/little,
      DPA_level:8/little,
      DPL_level:8/little,
      UnknownSection:16#286/binary,
      SongID:32/little,
      Volume:32/little,
      SPB_ident:8/little,
      SPN_ident:8/little,
      SPH_ident:8/little,
      SPA_ident:8/little,
      SPL_ident:8/little,
      DPB_ident:8/little,
      DPN_ident:8/little,
      DPH_ident:8/little,
      DPA_ident:8/little,
      DPL_ident:8/little,
      BGA_delay:16/little,
      BGA_filename:16#20/binary,
      AFP_flag:32/little,
      AFP_data:16#140/binary,
      UnknownSection2:4/binary>>.
