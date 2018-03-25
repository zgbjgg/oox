-module(oox_scheduler).

-behaviour(gen_server).

%% API
-export([start_link/0,
    stop_link/0,
    add_job/1,
    start_job/2,
    get_jobs/0,
    set_subscriber/1,
    get_subscriber/0,
    get_queue/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {jobs = [] :: list(), queue = [] :: list(),
    subscriber = undefined :: undefined | pid()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_link() ->
    gen_server:call(?MODULE, stop_link).

add_job(Host) ->
    gen_server:call(?MODULE, {add_job, Host}).

start_job(Job, Commands) ->
    gen_server:call(?MODULE, {start_job, Job, Commands}).

get_jobs() ->
    gen_server:call(?MODULE, get_jobs).

get_subscriber() ->
    gen_server:call(?MODULE, get_subscriber).

set_subscriber(Subscriber) ->
    gen_server:call(?MODULE, {set_subscriber, Subscriber}).

get_queue() ->
    gen_server:call(?MODULE, get_queue).

init([]) ->
    {ok, #state{jobs = [], subscriber = undefined}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call({add_job, Host}, _From, State) ->
    Scheduler = self(),
    % create a new job and setup in the server
    {ok, Job} = oox_job:start_link(Scheduler, Host),
    % right after created job, launch to pass in ready state
    ok = gen_server:call(Job, launch),
    {reply, {ok, Job}, State};

handle_call({start_job, Job, Commands}, _From, State=#state{jobs = Jobs, queue = Queue}) ->
    case lists:member(Job, Jobs) of
        true  ->
            ok = gen_server:cast(Job, {exe, Commands}),
            {reply, {ok, working}, State};
        false ->
            % maybe this request arrives when slave is not ready,
            % just add to the queue and if error, out from there!  
            {reply, {ok, queuing}, State#state{queue = [{Job, Commands} | Queue]}}
    end;

handle_call(get_jobs, _From, State=#state{jobs = Jobs}) ->
    {reply, {ok, Jobs}, State};

handle_call(get_queue, _From, State=#state{queue = Queue}) ->
    {reply, {ok, Queue}, State};

handle_call({set_subscriber, Subscriber}, _From, State) ->
    {reply, ok, State#state{subscriber = Subscriber}};

handle_call(get_subscriber, _From, State=#state{subscriber = Subscriber}) ->
    {reply, {ok, Subscriber}, State};

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

handle_cast({oox, up, Job}, State=#state{jobs = Jobs, queue = Queue}) ->
    % maybe job was already in queue, so execute inmediatly the exec commands
    NQueue = case lists:keyfind(Job, 1, Queue) of
        {Job, Commands} ->
            ok = gen_server:cast(Job, {exe, Commands}),
            lists:keydelete(Job, 1, Queue); 
        false           ->
            Queue
    end,
    {noreply, State#state{jobs = [Job | Jobs], queue = NQueue}};

handle_cast({oox, error, Job, _Error}, State=#state{queue = Queue}) ->
    % remove from queue if any
    NQueue = lists:keydelete(Job, 1, Queue),
    {noreply, State#state{queue = NQueue}};

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

