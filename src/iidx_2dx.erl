-module(iidx_2dx).

-export([decode/1]).
-export([encode/1]).

-include_lib("stdlib/include/assert.hrl").

-define(MAGIC, "2DX9").
-define(TWODX_HEADER_SIZE, 72).
-define(WAV_HEADER_SIZE, 24).

%--- API -----------------------------------------------------------------------

decode(Binary) ->
    {Name, Offsets} = decode_header(Binary),
    WavFiles = extract_wav_files(Name, Offsets, Binary),
    #{name => Name, files => WavFiles}.

encode(#{name := Name, files := WavFiles}) ->
    WavBinaries = encode_wav_files(WavFiles),
    Header = encode_header(Name, WavBinaries),
    iolist_to_binary([Header, WavBinaries]).

%--- Internals -----------------------------------------------------------------

% DECODING

decode_header(<<Header:?TWODX_HEADER_SIZE/binary, Rest/binary>>) ->
    <<NameSection:16/binary,
      HeaderSize:32/little,
      NumFiles:32/little,
      _:48/binary>> = Header,
    OffsetsSize = (4 * NumFiles),
    ?assert(HeaderSize == (?TWODX_HEADER_SIZE + OffsetsSize)),
    [Name, _Unk] = binary:split(NameSection, <<0>>),
    <<OffsetsBin:OffsetsSize/binary, _/binary>> = Rest,
    FileOffsets = [Offset || <<Offset:32/little>> <= OffsetsBin],
    {Name, FileOffsets}.

extract_wav_files(Name, Offsets, Binary) ->
    IndexedOffsets = lists:zip(lists:seq(1, length(Offsets)), Offsets),
    lists:foldl(fun({I, Offset}, WavFiles) ->
        WavHeader = binary:part(Binary, Offset, ?WAV_HEADER_SIZE),
        <<?MAGIC,
          ?WAV_HEADER_SIZE:32/little,
          WavSize:32/little,
          _:16/little,
          Track:16/unsigned-little,
          _:16/little,
          Attenuation:16/unsigned-little,
          Loop:32/little>> = WavHeader,
        WavDataOffset = Offset + ?WAV_HEADER_SIZE,
        WavData = binary:part(Binary, WavDataOffset, WavSize),
        Filename = iolist_to_binary([Name, "_", integer_to_list(I), ".wav"]),
        FileData = #{track => Track,
                     attenuation => Attenuation,
                     loop => Loop,
                     data => WavData},
        maps:put(Filename, FileData, WavFiles)
    end,
    #{},
    IndexedOffsets).

% ENCODING

encode_wav_files(WavMap) ->
    FilesContent = [Info || {_Name, Info} <- lists:sort(maps:to_list(WavMap))],
    Binaries = lists:foldl(fun pack_wav/2, [], FilesContent),
    lists:reverse(Binaries).

pack_wav(#{data := WavBinary, track := Track,
           attenuation := Attenuation, loop := Loop}, Binaries) ->
    PackedWav = <<?MAGIC,
                  ?WAV_HEADER_SIZE:32/little,
                  (size(WavBinary)):32/little,
                  0:16/little,
                  Track:16/unsigned-little,
                  0:16/little,
                  Attenuation:16/unsigned-little,
                  Loop:32/little,
                  WavBinary/binary>>,
    [PackedWav | Binaries].

encode_header(Name, WavBinaries) ->
    NumFiles = length(WavBinaries),
    HeaderSize = ?TWODX_HEADER_SIZE + (4 * NumFiles),
    NameSection = <<Name/binary, 0:((16 - size(Name)) * 8)/little>>,
    Header = <<NameSection/binary,
               HeaderSize:32/little,
               NumFiles:32/little, 0:(48*8)>>,
    Offsets = calculate_offsets(WavBinaries),
    EncodedOffsets = iolist_to_binary([<<O:32/little>> || O <- Offsets]),
    <<Header/binary, EncodedOffsets/binary>>.

calculate_offsets(WavBinaries) ->
    % The first offset is predictable,
    % The last Packed wav is not used to calculate the offsets
    FirstOffset = ?TWODX_HEADER_SIZE + 4,
    RelevantBinaries = lists:droplast(WavBinaries),
    Offsets = lists:foldl(
        fun(Bin, [LastOffset| _] = Offsets) ->
            Size = byte_size(Bin),
            [LastOffset + Size | Offsets]
        end,
        [FirstOffset],
        RelevantBinaries),
    lists:reverse(Offsets).
