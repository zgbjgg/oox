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

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({launch_slave, Caller}, State=#state{slaves = Slaves, hostname = Host}) ->
    % launch the slave, since this library only contains the
    % slaves startup & management ensure that code path
    % also loads the jun environment or at least is started
    JunApp = application:ensure_started(jun),
    lager:info("ensuring started for jun main dependency, state ~p", [JunApp]),

    % get full code path
    CodePath = code:get_path(),

    % now start slave and ensure all is started correctly!
    SlaveSerial = oox_slave:unique_serial(),
    Options = oox_slave:set_options(),
    case slave:start(Host, SlaveSerial, Options) of
        {ok, SlaveNode} ->
            % launch code into new slave
            ok = rpc:call(SlaveNode, code, add_paths, [CodePath]),
            % insert slave into slaves (ets) & publish slave to caller
            true = ets:insert(Slaves, {SlaveSerial, SlaveNode}),
            Caller ! {ok, SlaveNode},
            {noreply, State};
        Error           ->
            lager:error("could not launch slave node, reason ~p", [Error]),
            Caller ! {error, Error},
            {noreply, State}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{slaves = Slaves}) ->
    % terminate each slave
    SlaveNodes = ets:tab2list(Slaves),
    ok = lists:foreach(fun({_, SlaveNode}) ->
        slave:stop(SlaveNode)
    end, SlaveNodes),
    true = ets:delete(Slaves), % remove slaves if all terminates!
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
