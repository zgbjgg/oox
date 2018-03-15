-module(oox_scheduler).

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
-record(state, {jobs = [] :: list(),
    subscriber = undefined :: undefined | pid()}).

start_link(Pid) ->
    gen_server:start_link(?MODULE, [Pid], []).

stop_link(Pid) ->
    gen_server:call(Pid, stop_link).

init([Pid]) ->
    {ok, #state{jobs = [], subscriber = Pid}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call({add_job, Host}, _From, State=#state{jobs = Jobs}) ->
    % create a new job and setup in the server
    {ok, Job} = oox_job:start_link(Host),
    % right after created job, launch to pass in ready state
    ok = gen_server:call(Job, launch),
    {reply, {ok, Job}, State#state{jobs = [Job | Jobs]}};

handle_call({start_job, Job, Commands}, _From, State=#state{jobs = Jobs}) ->
    Pid = self(),
    case lists:member(Job, Jobs) of
        true  ->
            ok = gen_server:cast(Job, {exe, Pid, Commands}),
            {reply, {ok, working}, State};
        false ->
            {reply, {error, no_such_job}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({oox, done_job, Job, Results}, State=#state{jobs = Jobs, subscriber = Subscriber}) ->
    % terminates the Job nicely
    ok = oox_job:stop_link(Job),
    % the subscribing process will receive a notification about how the job was
    % based on the results analisis
    JobStatus = analyze_results(Results),
    Subscriber ! {job, Job, JobStatus},
    {noreply, State#state{jobs = Jobs -- [Job]}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================

analyze_results([])                    ->
    passed;
analyze_results([{error, Error} | _R]) ->
    {broken, {error, Error}};
analyze_results([_ | R])               ->
    analyze_results(R).

