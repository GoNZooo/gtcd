-module(gtcd_bencoding).

-export([encode/1, decode/1]).

encode(Value) when is_binary(Value); is_integer(Value); is_list(Value); is_map(Value) ->
  {ok, gtcd_bencoding_encoding:encode(Value)};
encode(Other) ->
  {error, {invalid_bencoding_value, type_of(Other)}}.

decode(Value) when is_binary(Value) ->
  gtcd_bencoding_decoding:decode(Value);
decode(Other) ->
  {error, {invalid_bencoding_value, type_of(Other)}}.

type_of(Value) when is_binary(Value) ->
  binary;
type_of(Value) when is_integer(Value) ->
  integer;
type_of(Value) when is_list(Value) ->
  list;
type_of(Value) when is_map(Value) ->
  map;
type_of(Value) when is_function(Value) ->
  function;
type_of(Value) when is_pid(Value) ->
  pid;
type_of(Value) when is_port(Value) ->
  port;
type_of(Value) when is_reference(Value) ->
  reference;
type_of(Value) when is_tuple(Value) ->
  tuple;
type_of(Value) when is_atom(Value) ->
  atom;
type_of(Value) when is_float(Value) ->
  float;
type_of(Value) when is_boolean(Value) ->
  boolean;
type_of(Value) when is_number(Value) ->
  number;
type_of(Value) when is_bitstring(Value) ->
  bitstring;
type_of(_Value) ->
  unknown.

