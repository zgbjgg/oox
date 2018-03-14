-module(oox_slave).

-export([unique_serial/0,
    set_options/0,
    set_options/1]).

unique_serial() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

set_options() ->
    set_options(erlang:get_cookie()).

set_options(Cookie) when is_atom(Cookie) ->
    set_options(atom_to_list(Cookie));
set_options(Cookie)                      ->
    "-setcookie " ++ Cookie.
