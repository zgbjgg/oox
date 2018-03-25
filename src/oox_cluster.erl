-module(oox_cluster).

-behaviour(gen_server).

%% API
-export([start_link/1,
    stop_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {slaves = undefined :: atom(),
    hostname :: string()}).

% default kill for os pid of each port (node)
-define(KILL(OsPid), "kill -9 " ++ integer_to_list(OsPid)).

start_link(Host) ->
    % normal startup without any slave
    gen_server:start_link(?MODULE, [Host], []).

stop_link(Pid) ->
    gen_server:call(Pid, stop_link).

init([Host]) ->
    % init the storage in ets for the slaves
    Slaves = ets:new(oox_slaves, [named_table, private]),
    {ok, #state{slaves = Slaves, hostname = Host}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call({rpc, SlaveNode, Options}, _From, State=#state{slaves = Slaves}) ->
    % search slave node in slaves
    case ets:lookup(Slaves, SlaveNode) of
        [{SlaveNode, _}] ->
            % simple execute the rpc call using the passed options
            Module = proplists:get_value(mod, Options),
            Function = proplists:get_value(func, Options),
            Args = proplists:get_value(args, Options, []),

            Response = rpc:call(SlaveNode, Module, Function, Args),

            lager:debug("sent RPC to slave node ~p with response ~p", [SlaveNode, Response]),

            {reply, Response, State};
        _                ->
            lager:error("could not find slave ~p into active oox slaves", [SlaveNode]),
            {reply, {error, notfound}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({launch_slave, Caller}, State=#state{slaves = Slaves, hostname = Host}) ->
    % launch the slave, since this library only contains the
    % slaves startup & management ensure that code path
    % also loads the jun environment or at least is started
    JunApp = application:ensure_started(jun),
    lager:info("ensuring started for jun main dependency, state ~p", [JunApp]),

    % get full code path
    % NOTE: when using path just load the required apps, since loading entire
    % path will cause a long delay starting the slave
    CodePath = lists:map(fun(App) ->
        code:lib_dir(App) ++ "/ebin"
    end, [goldrush, lager, erlport, jun]),

    % now start slave and ensure all is started correctly!
    SlaveSerial = oox_slave:unique_serial(),
    Nodename = integer_to_list(SlaveSerial) ++ "@" ++ Host, 
    Options = oox_slave:set_options(Nodename, CodePath),
    case oox_slave:node_port(Options, 1000) of
        {ok, Port} ->
            % call to net adm to ping new node
            case oox_slave:ensure_ready(Nodename) of
                {ok, SlaveNode}      ->
                    lager:info("reached slave node ~p, sending launch signal to main process ~p", [SlaveNode, Caller]),
                    % insert slave into slaves (ets) & publish slave to caller
                    true = ets:insert(Slaves, {SlaveNode, {SlaveSerial, Port}}),
                    Caller ! {oox, launch, SlaveNode},
                    {noreply, State};
                {error, _}=Error ->
                    lager:error("could not reach slave node, reason ~p", [Error]),
                    Caller ! {oox, error, Error},
                    OsPid = proplists:get_value(os_pid, erlang:port_info(Port)),
                    os:cmd(?KILL(OsPid)),
                    {noreply, State}
            end;
        Error           ->
            lager:error("could not launch slave node, reason ~p", [Error]),
            Caller ! {oox, error, Error},
            {noreply, State}
    end;

handle_cast({stop_slave, SlaveNode}, State=#state{slaves = Slaves}) ->
    % remove from active oox slaves
    [{_, {_, Port}}] = ets:lookup(Slaves, SlaveNode),
    true = ets:delete(Slaves, SlaveNode),
    OsPid = proplists:get_value(os_pid, erlang:port_info(Port)),
    os:cmd(?KILL(OsPid)),
    true = port_close(Port),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{slaves = Slaves}) ->
    % terminate each slave
    SlaveNodes = ets:tab2list(Slaves),
    ok = lists:foreach(fun({_SlaveNode, {_, Port}}) ->
        OsPid = proplists:get_value(os_pid, erlang:port_info(Port)),
        os:cmd(?KILL(OsPid)),
        port_close(Port)
    end, SlaveNodes),
    true = ets:delete(Slaves), % remove slaves if all terminates!
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
