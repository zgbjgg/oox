-module(oox_job).

-behaviour(gen_server).

%% API
-export([start_link/2,
    stop_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {cluster = undefined :: undefined | pid(),
    slave = down :: down | atom(),
    state = init :: atom(),
    scheduler = undefined :: undefined | pid(),
    jun_worker = undefined :: undefined | pid()}).

start_link(Scheduler, Host) ->
    gen_server:start_link(?MODULE, [Scheduler, Host], []).

stop_link(Pid) ->
    gen_server:call(Pid, stop_link).

init([Scheduler, Host]) ->
    % at init process start the cluster, at this point
    % since launching a new slave is an asynchronous process
    % then let it do after initializing
    {ok, Cluster} = oox_cluster:start_link(Host),
    {ok, #state{cluster = Cluster, slave = down, state = started, scheduler = Scheduler}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call(launch, _From, State=#state{cluster = Cluster, state = started}) ->
    % launch a slave in the cluster
    Pid = self(),
    ok = gen_server:cast(Cluster, {launch_slave, Pid}),
    {reply, ok, State#state{state = launching}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({exe, Commands}, State=#state{cluster = Cluster,
        slave = SlaveNode, state = ready, scheduler = Scheduler,
        jun_worker = Worker}) ->
    Pid = self(),
    % in the first parsing just check for `$worker`
    ParsedCommands = oox_slave:parse_commands(Commands, '$worker', Worker),
    Results = lists:foldl(fun(Cmd, Acc) ->
        Last = oox_slave:last_dataframe(lists:reverse(Acc)),
        [Cmd0] = oox_slave:parse_commands([Cmd], '$dataframe', Last),
        case gen_server:call(Cluster, {rpc, SlaveNode, Cmd0}) of
            {ok, R}        ->
                Acc ++ [R];
            {error, Error} ->
                Acc ++ [{error, Error}]
         end
    end, [], ParsedCommands),
    % if results contains some error in a command, emit the incident
    % to the scheduler, otherwise emit a success execution of this job in
    % order to scheduler stop it.
    ok = gen_server:cast(Scheduler, {oox, done_job, Pid, Results}),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({oox, launch, SlaveNode}, State=#state{cluster = Cluster,
        state = launching, scheduler = Scheduler}) ->
    lager:info("receiving launch signal for slave ~p", [SlaveNode]),
    % start a new worker for jun in the slave through cluster
    Options = [{mod, jun_worker},
        {func, start_link},
        {args, []}],
    {ok, Worker} = gen_server:call(Cluster, {rpc, SlaveNode, Options}),
    lager:info("starting jun worker on slave node at ~p", [Worker]),
    % send back to scheduler that all was ok
    ok = gen_server:cast(Scheduler, {oox, up, self()}),
    % setup the slave in the job
    {noreply, State#state{cluster = Cluster, slave = SlaveNode,
        state = ready, jun_worker = Worker}};

handle_info({oox, error, Error}, State=#state{state = launching, scheduler = Scheduler}) ->
    % come back to scheduler, some happens!
    lager:error("could not start & reach slave, reason ~p", [Error]),
    ok = gen_server:cast(Scheduler, {oox, error, self(), Error}),
    {noreply, State#state{state = down}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cluster = Cluster}) ->
    ok = oox_cluster:stop_link(Cluster),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
