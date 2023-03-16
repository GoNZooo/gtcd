-module(gtcd_http).

-export([announce/2, construct_url/2]).

announce(Url,
         #{info_hash := _InfoHash,
           peer_id := _PeerId,
           port := _Port,
           ip := _Ip,
           downloaded := _Downloaded,
           uploaded := _Uploaded,
           left := _Left,
           event := _Event} =
           Parameters) ->
  UrlWithQuery = construct_url(Url, Parameters),
  Result =
    case httpc:request(get, {UrlWithQuery, []}, [], []) of
      {ok, {_HttpData, _Headers, Body}} when is_list(Body) ->
        gtcd_bencoding_decoding:decode(list_to_binary(Body));
      {ok, {_HttpData, _Headers, Body}} when is_binary(Body) ->
        gtcd_bencoding_decoding:decode(Body);
      {error, HttpReason0} ->
        {error, HttpReason0}
    end,
  % @TODO: This only handles the compact form of the `peers` value
  case Result of
    {ok, #{value := #{<<"failure reason">> := FailureReason}}} ->
      {error, {announce_error, FailureReason}};
    {ok, #{value := #{<<"peers">> := PeersBinary, <<"interval">> := Interval}}} ->
      {ok, #{peers => parse_peers(PeersBinary), interval => Interval}};
    {error, HttpReason} ->
      {error, {http_error, HttpReason}}
  end.

construct_url(Url,
              #{info_hash := InfoHash,
                peer_id := PeerId,
                port := Port,
                ip := Ip,
                downloaded := Downloaded,
                uploaded := Uploaded,
                left := Left,
                event := Event}) ->
  UrlEncodedPeerId = list_to_binary(uri_string:quote(binary_to_list(PeerId))),
  PortAsString = integer_to_binary(Port),
  DownloadedAsString = integer_to_binary(Downloaded),
  UploadedAsString = integer_to_binary(Uploaded),
  LeftAsString = integer_to_binary(Left),
  case uri_string:parse(Url) of
    #{scheme := _Scheme,
      host := _Host,
      path := _Path} =
      ParsedUri ->
      Uri =
        add_to_query(ParsedUri,
                     [{<<"info_hash">>, InfoHash},
                      {<<"peer_id">>, UrlEncodedPeerId},
                      {<<"port">>, PortAsString},
                      {<<"ip">>, Ip},
                      {<<"downloaded">>, DownloadedAsString},
                      {<<"uploaded">>, UploadedAsString},
                      {<<"left">>, LeftAsString},
                      {<<"event">>, Event}]),
      uri_string:recompose(Uri);
    _Other ->
      {error, invalid_url}
  end.

add_to_query(#{query := Query0} = ParsedUri, Parameters) ->
  Query =
    lists:foldl(fun({Key, Value}, Q) -> <<Q/binary, "&", Key/binary, "=", Value/binary>> end,
                Query0,
                Parameters),
  ParsedUri#{query => Query}.

parse_peers(PeersBinary) ->
  parse_peers(PeersBinary, []).

parse_peers(<<>>, Peers) ->
  Peers;
parse_peers(<<Ip0:8, Ip1:8, Ip2:8, Ip3:8, Port:16, Rest/binary>>, Peers) ->
  Ip = {Ip0, Ip1, Ip2, Ip3},
  parse_peers(Rest, [{Ip, Port} | Peers]).
