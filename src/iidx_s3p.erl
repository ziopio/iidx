-module(iidx_s3p).

-export([decode/1]).
-export([encode/1]).

-define(S3P, "S3P0").
-define(S3V, "S3V0").
-define(ASF_HEADER_SIZE, 32).

%--- API -----------------------------------------------------------------------

decode(Binary) ->
    Offsets = decode_header(Binary),
    decode_s3v(Binary, Offsets).

encode(S3VFiles) ->
    % Encode the S3P header
    S3VData = [encode_s3v(S3V) || S3V <- S3VFiles],
    Header = encode_header(S3VData),
    <<Header/binary, (list_to_binary(S3VData))/binary>>.

%--- Internals -----------------------------------------------------------------

% DECODING

decode_header(<<?S3P, NumS3V:32/little, Rest/binary>>) ->
    <<Offsets:(NumS3V*8)/binary, _/binary>> = Rest,
    [{Offset, Size} || <<Offset:32/little, Size:32/little>> <= Offsets].

decode_s3v(Binary, Offsets) ->
    S3Vs = lists:foldl(
        fun({Offset, Size}, S3V) ->
            <<S3VBin:Size/binary>> = binary:part(Binary, Offset, Size),
            S3VData = decode_s3v_data(S3VBin),
            [S3VData|S3V]
        end,
        [],
        Offsets),
    lists:reverse(S3Vs).

decode_s3v_data(<<?S3V,
                  ?ASF_HEADER_SIZE:32/little,
                  S3VSize:32/little,
                  Unk:4/binary, % unknown
                  _:16/binary, % usually is all 0
                  Data/binary>>) ->
    <<S3VBinary:S3VSize/binary, _/binary>> = Data,
    #{unk => Unk, data => S3VBinary}.

% ENCODING

encode_s3v(#{unk := Unk, data := Binary}) ->
    S3VSize = byte_size(Binary),
    <<?S3V, ?ASF_HEADER_SIZE:32/little,
      S3VSize:32/little, Unk:4/binary, 0:(16*8), Binary/binary>>.

encode_header(S3VData) ->
    Offsets = encode_offsets(S3VData),
    <<?S3P, (length(S3VData)):32/little, Offsets/binary>>.

encode_offsets([FirstBin|OtherS3Vs] = S3VData) ->
    NumFiles = length(S3VData),
    FirstOffset = 8 + NumFiles * 8,
    Offsets = lists:foldl(
        fun(Bin, [{LastOffset, LastSize}| _] = Offsets) ->
            Offset = LastOffset + LastSize,
            Size = byte_size(Bin),
            [{Offset, Size} | Offsets]
        end,
        [{FirstOffset, size(FirstBin)}],
        OtherS3Vs),
    OffsetBins = [<<O:32/little, S:32/little>>
                  || {O,S} <- lists:reverse(Offsets)],
    list_to_binary(OffsetBins).
