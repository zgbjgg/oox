-module(oox_slave).

-export([unique_serial/0,
    set_options/0,
    set_options/1,
    parse_commands/3]).

unique_serial() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

set_options() ->
    set_options(erlang:get_cookie()).

set_options(Cookie) when is_atom(Cookie) ->
    set_options(atom_to_list(Cookie));
set_options(Cookie)                      ->
    "-setcookie " ++ Cookie.

% for now parsing commands is a great helper to set
% the worker in every command, using a single syntax from
% oox client.
parse_commands([], _For, _Value)                 -> [];
parse_commands(Commands, _For, none)             -> Commands;
parse_commands([Command | Commands], For, Value) ->
    Args = proplists:get_value(args, Command, []),
    F = build_fun(For, Value),
    NewArgs = lists:map(F, Args),
    NewCommand = proplists:delete(args, Command) ++ [{args, NewArgs}],
    [NewCommand] ++ parse_commands(Commands, For, Value).

%% @hidden

build_fun('$worker', Value)    ->
    fun('$worker') -> Value;
       (A)         -> A
    end;
build_fun('$dataframe', Value) ->
    fun('$dataframe') -> {_, DataFrame} = Value, DataFrame;
       (A)            -> A
    end.
