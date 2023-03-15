-module(gtcd_metainfo).

-export([create/1, parse_torrent_file/1, parse_torrent_data/1]).

parse_torrent_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} -> parse_torrent_data(Data);
    {error, Reason} -> {error, Reason}
  end.

parse_torrent_data(Data) ->
  case gtcd_bencoding:decode(Data) of
    {ok, #{value := ParsedData}} ->
      create(ParsedData);
    {error, _} ->
      {error, invalid_torrent_file}
  end.

% File metainfo
create(#{<<"info">> :=
           #{<<"length">> := Length,
             <<"name">> := FileName,
             <<"piece length">> := PieceLength,
             <<"pieces">> := Pieces} =
             Info} =
         Data) ->
  {ok,
   {file_metainfo,
    #{announce => get_announce_url(Data),
      length => Length,
      name => FileName,
      piece_length => PieceLength,
      pieces => Pieces,
      info_hash => hash_metainfo_object(gtcd_bencoding_encoding:encode(Info))}}};
% Directory metainfo
create(#{<<"info">> :=
           #{<<"files">> := Files,
             <<"name">> := DirectoryName,
             <<"piece length">> := PieceLength,
             <<"pieces">> := Pieces} =
             Info} =
         Data) ->
  {ok,
   {directory_metainfo,
    #{announce => get_announce_url(Data),
      files => create_files_object(Files),
      name => DirectoryName,
      piece_length => PieceLength,
      pieces => Pieces,
      info_hash => hash_metainfo_object(gtcd_bencoding_encoding:encode(Info))}}}.

get_announce_url(#{<<"announce">> := Announce}) ->
  [Announce];
get_announce_url(#{<<"url-list">> := UrlList}) ->
  UrlList.

create_files_object(Files) ->
  lists:map(fun create_file_object/1, Files).

create_file_object(#{<<"length">> := Length, <<"path">> := Path}) ->
  #{length => Length, path => Path}.

hash_metainfo_object(Binary) ->
  to_hex(crypto:hash(sha, Binary)).

to_hex(Binary) ->
  << <<(list_to_binary(io_lib:format("~2.16.0b", [X])))/binary>> || <<X>> <= Binary >>.
