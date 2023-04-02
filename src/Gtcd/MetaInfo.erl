-module(gtcd_metaInfo@foreign).

-export([parseTorrentFile/1]).

parseTorrentFile(Path) ->
  fun() ->
     case gtcd_metainfo:parse_torrent_file(Path) of
       {ok,
        {file_metainfo,
         #{info := #{piece_length := PieceLength} = Info, info_hash := InfoHash} = Map}} ->
         Announce = get_announce(Map),
         {right,
          #{announce => Announce,
            info => {fileInfoMap, Info#{pieceLength => PieceLength}},
            info_hash => InfoHash}};
       {ok,
        {directory_metainfo,
         #{info := #{piece_length := PieceLength} = Info, info_hash := InfoHash} = Map}} ->
         Announce = get_announce(Map),
         {right,
          #{announce => Announce,
            info => {multiFileInfoMap, Info#{pieceLength => PieceLength}},
            info_hash => InfoHash}};
       {error, Posix} -> {left, {posix, Posix}}
     end
  end.

get_announce(#{announce := Announce}) when is_list(Announce) ->
  {announceList, Announce};
get_announce(#{announce := Announce}) ->
  {announceString, Announce}.
