-module(bencoding_decoding).

-export([decode/1]).

decode(<<"i", Rest/binary>>) ->
  case decode_integer(Rest) of
    {parsed, Payload} ->
      {ok, Payload};
    {parse_error, invalid} ->
      {error, invalid_integer}
  end;
decode(<<"l", Rest/binary>>) ->
  case decode_list(Rest) of
    {parsed, Payload} ->
      {ok, Payload};
    {parse_error, invalid} ->
      {error, invalid_list}
  end;
decode(<<"d", Rest/binary>>) ->
  case decode_map(Rest) of
    {parsed, Payload} ->
      {ok, Payload};
    {parse_error, invalid} ->
      {error, invalid_map}
  end;
decode(Binary) ->
  case decode_string(Binary) of
    {parsed, Payload} ->
      {ok, Payload};
    {parse_error, invalid} ->
      {error, invalid_string}
  end.

decode_integer(Binary) ->
  case binary:split(Binary, <<"e">>, []) of
    [IntegerAsString, Rest] ->
      {parsed, #{value => binary_to_integer(IntegerAsString), rest => Rest}};
    _Other ->
      {parse_error, invalid}
  end.

decode_string(Binary) ->
  case binary:match(Binary, <<":">>, []) of
    nomatch ->
      {parse_error, invalid};
    {Index, 1} ->
      Size = binary_to_integer(binary:part(Binary, 0, Index)),
      String = binary:part(Binary, Index + 1, Size),
      Rest = binary:part(Binary, Index + Size + 1, byte_size(Binary) - Index - Size - 1),
      {parsed, #{value => String, rest => Rest}}
  end.

decode_list(Binary) ->
  decode_list(Binary, []).

decode_list(<<"e", Rest/binary>>, Acc) ->
  {parsed, #{value => lists:reverse(Acc), rest => Rest}};
decode_list(Binary, Acc) ->
  case decode(Binary) of
    {ok, #{value := Value, rest := Rest}} ->
      decode_list(Rest, [Value | Acc]);
    {error, _} ->
      {parse_error, invalid}
  end.

decode_map(Binary) ->
  decode_map(Binary, []).

decode_map(<<"e", Rest/binary>>, Acc) ->
  {parsed, #{value => maps:from_list(lists:reverse(Acc)), rest => Rest}};
decode_map(Binary, Acc) ->
  case decode(Binary) of
    {ok, #{value := Key, rest := Rest0}} ->
      case decode(Rest0) of
        {ok, #{value := Value, rest := Rest}} ->
          decode_map(Rest, [{Key, Value} | Acc]);
        {error, _} ->
          {parse_error, invalid}
      end;
    {error, _} ->
      {parse_error, invalid}
  end.

