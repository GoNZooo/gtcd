-module(gtcd_bencoding_encoding).

-export([encode/1]).

encode(String) when is_binary(String) ->
  encode_string(String);
encode(Integer) when is_integer(Integer) ->
  encode_integer(Integer);
encode(List) when is_list(List) ->
  encode_list(List);
encode(Map) when is_map(Map) ->
  encode_map(Map).

encode_string(String) ->
  Length = integer_to_binary(byte_size(String)),
  <<Length/binary, ":", String/binary>>.

encode_integer(Integer) ->
  IntegerAsString = integer_to_binary(Integer),
  <<"i", IntegerAsString/binary, "e">>.

encode_list(List) ->
  ListContents = encode_list_contents(List, []),
  ListAsString = join(ListContents),
  <<"l", ListAsString/binary, "e">>.

encode_list_contents([], Acc) ->
  lists:reverse(Acc);
encode_list_contents([H | T], Acc) ->
  encode_list_contents(T, [encode(H) | Acc]).

encode_map(Map) ->
  MapValues =
    lists:sort(fun({K1, _}, {K2, _}) -> value_as_string(K1) =< value_as_string(K2) end,
               maps:to_list(Map)),
  MapContents = encode_map_contents(MapValues, []),
  MapAsString = join(lists:flatten(MapContents)),
  <<"d", MapAsString/binary, "e">>.

encode_map_contents([], Acc) ->
  lists:reverse(Acc);
encode_map_contents([{Key, Value} | T], Acc) ->
  encode_map_contents(T, [[encode(Key), encode(Value)] | Acc]).

value_as_string(Value) when is_binary(Value) ->
  Value;
value_as_string(Value) when is_integer(Value) ->
  integer_to_binary(Value);
value_as_string(Value) when is_list(Value) ->
  <<"[", (lists:join(<<",">>, lists:map(fun value_as_string/1, Value)))/binary, "]">>;
value_as_string(Value) when is_map(Value) ->
  <<"{",
    (lists:join(<<",">>, lists:map(fun value_as_string/1, maps:to_list(Value)))),
    "}">>.

join(List) ->
  intercalate(List, <<>>).

intercalate([], _Separator) ->
  <<>>;
intercalate([H], _Separator) ->
  H;
intercalate([H | T], Separator) ->
  <<H/binary, Separator/binary, (intercalate(T, Separator))/binary>>.
