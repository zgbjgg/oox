-module(oox_slave).

-export([unique_serial/0,
    set_options/2,
    parse_commands/3,
    last_dataframe/1,
    node_port/2,
    ensure_ready/1,
    last_series/1,
    last_iplot/1]).

-define(CLASS_DATAFRAME, 'pandas.core.frame.DataFrame').
-define(CLASS_SERIES, 'pandas.core.frame.Series').
-define(CLASS_IPLOT, 'plotly.iplot').
-define(CLASS_GROUPBY, 'pandas.core.groupby.DataFrameGroupBy').

unique_serial() ->
    {MS, S, US} = erlang:timestamp(),
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
    Args = case lists:keyfind(args, 1, Command) of false -> []; {args, C} -> C end,
    F = build_fun(For, Value),
    NewArgs = lists:map(F, Args),
    NewCommand = lists:keydelete(args, 1, Command) ++ [{args, NewArgs}],
    [NewCommand] ++ parse_commands(Commands, For, Value).

% check where resides the last dataframe to take from there
last_dataframe([])                                  -> none;
last_dataframe([{?CLASS_GROUPBY, DataFrame} | _])   ->
    {?CLASS_GROUPBY, DataFrame};
last_dataframe([{?CLASS_DATAFRAME, DataFrame} | _]) ->
    {?CLASS_DATAFRAME, DataFrame};
last_dataframe([_ | Rs])                            ->
    last_dataframe(Rs).

% check where resides the last series to take from there
last_series([])                            -> none;
last_series([{?CLASS_SERIES, Series} | _]) ->
    {?CLASS_SERIES, Series};
last_series([_ | Rs])                      ->
    last_series(Rs).

% check where resides the last iplot to take and make an url
last_iplot([])                          -> none;
last_iplot([{?CLASS_IPLOT, IPlot} | _]) ->
    {?CLASS_IPLOT, IPlot};
last_iplot([_ | Rs])                    ->
    last_iplot(Rs).

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
    end;
build_fun('$series', Value)   ->
    fun('$series') -> {_, Series} = Value, Series;
       (A)         -> A
    end;
build_fun('$iplot', Value)    ->
    fun('$iplot') -> {_, IPlot} = Value, IPlot;
       (A)        -> A
    end.
