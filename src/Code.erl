-module(code@foreign).

-export([privDir/1]).

privDir(Name) ->
  fun() ->
     case binary_to_maybe_atom(Name) of
       {ok, App} ->
         case code:priv_dir(App) of
           Path when is_list(Path) -> {just, list_to_binary(Path)};
           {error, bad_name} -> {nothing}
         end;
       {error, not_an_existing_atom} -> {nothing}
     end
  end.

binary_to_maybe_atom(Binary) ->
  try binary_to_existing_atom(Binary) of
    Atom ->
      {ok, Atom}
  catch
    error:badarg ->
      {error, not_an_existing_atom}
  end.
