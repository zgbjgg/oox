-module(oox_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_oox_app/1,
    test_oox_scheduler_get_jobs/1,
    test_oox_scheduler_set_subscriber/1,
    test_oox_scheduler_get_subscriber/1,
    test_oox_scheduler_add_job/1,
    test_oox_scheduler_add_job_broken/1,
    test_oox_scheduler_add_job_bad_host/1]).

-define(HOST, "127.0.0.1").
-define(BAD_HOST, "").
-define(CMDS(Path), [[{mod,jun_pandas},
    {func,read_csv},
    {args,['$worker',Path,[]]}],
   [{mod,jun_pandas},
    {func,head},
    {args,['$worker','$dataframe',1,[]]}]]).
-define(BROKEN_CMDS(Path), [[{mod,jun_pandas},
    {func,read_csv},
    {args,['$worker',Path,[]]}],
   [{mod,jun_pandas},
    {func,max},
    {args,['$worker','$dataframe',broken_age,[]]}]]).


all() ->
    [test_oox_app,
     test_oox_scheduler_get_jobs,
     test_oox_scheduler_set_subscriber,
     test_oox_scheduler_get_subscriber,
     test_oox_scheduler_add_job,
     test_oox_scheduler_add_job_broken,
     test_oox_scheduler_add_job_bad_host].

init_per_testcase(_, _Config) ->
    % start all deps
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(erlport),
    ok = application:start(jun),
    % start the oox node (master)
    net_kernel:start(['oox@127.0.0.1', longnames]),
    true = erlang:set_cookie(node(), 'oox'),
    % define commands since can be used path for file
    {ok, Cwd} = file:get_cwd(),
    Path = list_to_binary(Cwd ++ "/../../lib/oox/test/files/csv.txt"),
    [{cmds, ?CMDS(Path)}, {broken_cmds, ?BROKEN_CMDS(Path)}].

end_per_testcase(_, _Config) ->
    ok = application:stop(compiler),
    ok = application:stop(syntax_tools),
    ok = application:stop(goldrush),
    ok = application:stop(lager),
    ok = application:stop(erlport),
    ok = application:stop(jun),
    ok = application:stop(oox),
    ok.

test_oox_app(_) ->
    ?assertEqual(ok, application:start(oox)).

test_oox_scheduler_get_jobs(_) ->
    ok = application:start(oox),
    {ok, Jobs} = oox_scheduler:get_jobs(),
    ?assertEqual([], Jobs).

test_oox_scheduler_set_subscriber(_) ->
    ok = application:start(oox),
    Ok = oox_scheduler:set_subscriber(self()),
    ?assertEqual(ok, Ok).

test_oox_scheduler_get_subscriber(_) ->
    ok = application:start(oox),
    Pid = self(),
    ok = oox_scheduler:set_subscriber(Pid),
    {ok, Subscriber} = oox_scheduler:get_subscriber(),
    ?assertEqual(Pid, Subscriber).

test_oox_scheduler_add_job(Config) ->
    ok = application:start(oox),
    % set subscriber to self, so in the last process we wait
    % until response is delivered from scheduler
    Pid = self(),
    ok = oox_scheduler:set_subscriber(Pid),
    % next create the job and sleep a minutes for slave initializing
    {ok, Job} = oox_scheduler:add_job(?HOST),
    ok = timer:sleep(3000),
    {ok, [Job]} = oox_scheduler:get_jobs(),
    % next send some commands (if broken the test will fails automatically)
    Cmds = proplists:get_value(cmds, Config),
    {ok, working} = oox_scheduler:start_job(Job, Cmds),
    receive 
        {job, Job, T, _Results} -> ?assertEqual(passed, T)
    after
        5000 ->
            ?assertEqual(timeout, Job)
    end.

test_oox_scheduler_add_job_broken(Config) ->
    ok = application:start(oox),
    % set subscriber to self, so in the last process we wait
    % until response is delivered from scheduler
    Pid = self(),
    ok = oox_scheduler:set_subscriber(Pid),
    % next create the job and sleep a minutes for slave initializing
    {ok, Job} = oox_scheduler:add_job(?HOST),
    ok = timer:sleep(3000),
    {ok, [Job]} = oox_scheduler:get_jobs(),
    Cmds = proplists:get_value(broken_cmds, Config),
    {ok, working} = oox_scheduler:start_job(Job, Cmds),
    receive
        {job, Job, T, _Results} -> ?assertMatch({broken, _}, T)
    after
        5000 ->
            ?assertEqual(timeout, Job)
    end.

test_oox_scheduler_add_job_bad_host(_) ->
    ok = application:start(oox),
    Pid = self(),
    ok = oox_scheduler:set_subscriber(Pid),
    {ok, _Job} = oox_scheduler:add_job(?BAD_HOST),
    % timer to simulate waiting for slave (never comes),
    % set a higher time due to net adm ping timeout!
    ok = timer:sleep(8000),
    {ok, Jobs} = oox_scheduler:get_jobs(),
    ?assertEqual([], Jobs).
