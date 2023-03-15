-module(gtcd_bencoding_decoding).

-export([decode/1, get_info_map/1]).

get_info_map(<<>>) ->
  {error, no_info_map};
get_info_map(<<"4:infod", Rest/binary>>) ->
  case decode_map(Rest) of
    {parsed, Payload} ->
      {ok, Payload};
    {parse_error, invalid} ->
      {error, invalid_info_map}
  end;
get_info_map(<<_X, Rest/binary>>) ->
  get_info_map(Rest).

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
      Source = <<"i", IntegerAsString/binary, "e">>,
      {parsed,
       #{value => binary_to_integer(IntegerAsString),
         rest => Rest,
         source => Source}};
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
      Source = <<(integer_to_binary(Size))/binary, ":", String/binary>>,
      {parsed,
       #{value => String,
         rest => Rest,
         source => Source}}
  end.

decode_list(Binary) ->
  decode_list(Binary, [], <<"l">>).

decode_list(<<"e", Rest/binary>>, Acc, Source) ->
  {parsed,
   #{value => lists:reverse(Acc),
     rest => Rest,
     source => <<Source/binary, "e">>}};
decode_list(Binary, Acc, Source) ->
  case decode(Binary) of
    {ok,
     #{value := Value,
       rest := Rest,
       source := ParsedSource}} ->
      decode_list(Rest, [Value | Acc], <<Source/binary, ParsedSource/binary>>);
    {error, _} ->
      {parse_error, invalid}
  end.

decode_map(Binary) ->
  decode_map(Binary, [], <<"d">>).

decode_map(<<"e", Rest/binary>>, Acc, Source) ->
  {parsed,
   #{value =>
       maps:from_list(
         lists:reverse(Acc)),
     rest => Rest,
     source => <<Source/binary, "e">>}};
decode_map(Binary, Acc, Source) ->
  case decode(Binary) of
    {ok,
     #{value := Key,
       rest := Rest0,
       source := KeySource}} ->
      case decode(Rest0) of
        {ok,
         #{value := Value,
           rest := Rest,
           source := ValueSource}} ->
          NewSource = <<Source/binary, KeySource/binary, ValueSource/binary>>,
          decode_map(Rest, [{Key, Value} | Acc], NewSource);
        {error, _} ->
          {parse_error, invalid}
      end;
    {error, _} ->
      {parse_error, invalid}
  end.
