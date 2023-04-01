-module(gtcd_metaInfo@foreign).

-export([parseTorrentFile/1]).

% type Torrent = { announce :: AnnounceData, info :: InfoMap, info_hash :: String }
%
% data AnnounceData
%   = AnnounceString String
%   | AnnounceList (List (List String))
%
% data InfoMap
%   = FileInfoMap { pieces :: List String, pieceLength :: Int, length :: Int }
%   | MultiFileInfoMap { pieces :: List String, pieceLength :: Int, files :: List FileInfo }

parseTorrentFile(Path) ->
  fun() ->
     case gtcd_metainfo:parse_torrent_file(Path) of
       {ok, {file_metainfo, #{info := Info, info_hash := InfoHash} = Map}} ->
         Announce = get_announce(Map),
         {right,
          #{announce => Announce,
            info => {fileInfoMap, Info},
            info_hash => InfoHash}};
       {ok, {directory_metainfo, #{info := Info, info_hash := InfoHash} = Map}} ->
         Announce = get_announce(Map),
         {right,
          #{announce => Announce,
            info => {multiFileInfoMap, Info},
            info_hash => InfoHash}};
       {error, Posix} -> {left, {posix, Posix}}
     end
  end.

get_announce(#{announce := Announce}) when is_list(Announce) ->
  {announceList, Announce};
get_announce(#{announce := Announce}) ->
  {announceString, Announce}.
