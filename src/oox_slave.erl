-module(oox_slave).

-export([unique_serial/0,
    set_options/2,
    parse_commands/3,
    last_dataframe/1,
    node_port/2,
    ensure_ready/1]).

unique_serial() ->
    {MS, S, US} = erlang:now(),
    (MS*1000000+S)*1000000+US.

set_options(Nodename, CodePath) ->
    set_options(Nodename, CodePath, erlang:get_cookie()).

set_options(Nodename, CodePath, Cookie) when is_atom(Cookie) ->
    set_options(Nodename, CodePath, atom_to_list(Cookie));
set_options(Nodename, CodePath, Cookie)                      ->
    Pa = string:join(CodePath, " "),
    BinDir = os:getenv("BINDIR"),
    lists:concat([BinDir, "/erl -name ", Nodename, " -noshell -noinput -setcookie ",
        Cookie, " -pa ", Pa]).

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

node_port(Cmd, Timeout) ->
    Port = open_port({spawn, Cmd}, [stream, exit_status]),
    receive
        {Port,{exit_status,_}} ->
            {error, exit_status}
    after Timeout ->
        {ok, Port}
    end.

ensure_ready(Nodename) when is_list(Nodename) ->
    ensure_ready(list_to_atom(Nodename));
ensure_ready(Nodename)                        ->
    case net_adm:ping(Nodename) of
        pong ->
            % now ensure the path is loaded!
            {ok, Nodename};
        pang ->
            {error, down}
    end.

%% @hidden

build_fun('$worker', Value)    ->
    fun('$worker') -> Value;
       (A)         -> A
    end;
build_fun('$dataframe', Value) ->
    fun('$dataframe') -> {_, DataFrame} = Value, DataFrame;
       (A)            -> A
    end.
