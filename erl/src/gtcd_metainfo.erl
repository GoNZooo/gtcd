-module(gtcd_metainfo).

-export([create/2, parse_torrent_file/1, parse_torrent_data/1]).

parse_torrent_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      parse_torrent_data(Data);
    {error, Reason} ->
      {error, Reason}
  end.

parse_torrent_data(Data) ->
  case gtcd_bencoding_decoding:get_info_map(Data) of
    {ok, #{source := Source}} ->
      InfoHash = to_hex(hash_metainfo_object(Source)),
      case gtcd_bencoding:decode(Data) of
        {ok, #{value := ParsedData}} ->
          create(ParsedData, InfoHash);
        {error, _} ->
          {error, invalid_torrent_file}
      end;
    {error, _} ->
      {error, invalid_torrent_file}
  end.

% File metainfo
create(#{<<"info">> := #{<<"length">> := Length}} = Data, InfoHash) ->
  case create_common_metainfo(Data, InfoHash) of
    {ok, MetaInfo} ->
      {ok, {file_metainfo, MetaInfo#{length => Length}}};
    {error, Reason} ->
      {error, Reason}
  end;
% Directory metainfo
create(#{<<"info">> := #{<<"files">> := Files0}} = Data, InfoHash) ->
  Files = create_files_object(Files0),
  case create_common_metainfo(Data, InfoHash) of
    {ok, MetaInfo} ->
      {ok, {directory_metainfo, MetaInfo#{files => Files}}};
    {error, Reason} ->
      {error, Reason}
  end.

create_common_metainfo(#{<<"info">> :=
                           #{<<"name">> := Name,
                             <<"piece length">> := PieceLength,
                             <<"pieces">> := PiecesBinary}} =
                         Data,
                       InfoHash) ->
  Announce = get_announce_url(Data),
  AdditionalData = get_additional_data(Data),
  case parse_pieces(PiecesBinary) of
    {ok, Pieces} ->
      {ok,
       #{announce => Announce,
         name => Name,
         piece_length => PieceLength,
         pieces => Pieces,
         info_hash => InfoHash,
         additional_data => AdditionalData}};
    {error, Reason} ->
      {error, Reason}
  end.

get_announce_url(#{<<"announce">> := Announce}) ->
  [Announce];
get_announce_url(#{<<"url-list">> := UrlList}) ->
  UrlList.

parse_pieces(PiecesBinary) when byte_size(PiecesBinary) rem 20 == 0 ->
  {ok, parse_pieces(PiecesBinary, [])};
parse_pieces(PiecesBinary) ->
  {error, {pieces_not_divisible_by_20, byte_size(PiecesBinary)}}.

parse_pieces(<<>>, Pieces) ->
  lists:reverse(Pieces);
parse_pieces(<<Hash:20/bytes, Rest/binary>>, Pieces) ->
  parse_pieces(Rest, [Hash | Pieces]).

create_files_object(Files) ->
  lists:map(fun create_file_object/1, Files).

create_file_object(#{<<"length">> := Length, <<"path">> := Path}) ->
  #{length => Length, path => Path}.

get_additional_data(Data) ->
  KeysToNotGet = [<<"announce">>, <<"info">>],
  maps:without(KeysToNotGet, Data).

hash_metainfo_object(Binary) ->
  crypto:hash(sha, Binary).

to_hex(Binary) ->
  << <<(list_to_binary(io_lib:format("%~2.16.0B", [X])))/binary>> || <<X>> <= Binary >>.
