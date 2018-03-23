-module(oox_slave).

-export([unique_serial/0,
    set_options/1,
    set_options/2,
    parse_commands/3,
    last_dataframe/1]).

unique_serial() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

set_options(CodePath) ->
    set_options(erlang:get_cookie(), CodePath).

set_options(Cookie, CodePath) when is_atom(Cookie) ->
    set_options(atom_to_list(Cookie), CodePath);
set_options(Cookie, CodePath)                      ->
    Pa = string:join(CodePath, " "),
    "-setcookie " ++ Cookie ++ " -pa " ++ Pa.

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

% check where resides the last dataframe to take from there
last_dataframe([])                       -> none;
last_dataframe([{Class, DataFrame} | _]) ->
    {Class, DataFrame};
last_dataframe([_ | Rs])                 ->
    last_dataframe(Rs).

%% @hidden

build_fun('$worker', Value)    ->
    fun('$worker') -> Value;
       (A)         -> A
    end;
build_fun('$dataframe', Value) ->
    fun('$dataframe') -> {_, DataFrame} = Value, DataFrame;
       (A)            -> A
    end.
