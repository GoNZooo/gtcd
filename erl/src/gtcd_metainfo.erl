-module(gtcd_metainfo).

-export([create/1, parse_torrent_file/1]).

parse_torrent_file(Path) ->
  {ok, Data} = file:read_file(Path),
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
             <<"pieces">> := Pieces},
         <<"announce">> := AnnounceUrl}) ->
  {ok,
   {file_metainfo,
    #{announce => AnnounceUrl,
      length => Length,
      name => FileName,
      piece_length => PieceLength,
      pieces => Pieces}}};
% Directory metainfo
create(#{<<"info">> :=
           #{<<"files">> := Files,
             <<"name">> := DirectoryName,
             <<"piece length">> := PieceLength,
             <<"pieces">> := Pieces},
         <<"announce">> := AnnounceUrl}) ->
  {ok,
   {directory_metainfo,
    #{announce => AnnounceUrl,
      files => create_files_object(Files),
      name => DirectoryName,
      piece_length => PieceLength,
      pieces => Pieces}}}.

create_files_object(Files) ->
  lists:map(fun create_file_object/1, Files).

create_file_object(#{<<"length">> := Length, <<"path">> := Path}) ->
  #{length => Length, path => Path}.
