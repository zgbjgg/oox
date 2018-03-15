# oox

**OOX** - slave isolated process & scheduler for jun environments


[![Build Status](https://travis-ci.org/zgbjgg/oox.svg?branch=master)](https://travis-ci.org/zgbjgg/oox)
[![Codecov](https://img.shields.io/codecov/c/github/zgbjgg/oox.svg)](https://codecov.io/gh/zgbjgg/oox)
[![License: MIT](https://img.shields.io/github/license/zgbjgg/oox.svg)](https://raw.githubusercontent.com/zgbjgg/oox/master/LICENSE)

OOX is a set of tools to execute many jun commands into an isolated slave process, taking control of the cluster (slaves) and managing
the connection to it. This tools also can be used to generate a job and check if passed or broken (similar to TDD) but applied to
jun commands and taking advantage of the slave to run all process on a separated node.

This project is under development and should not be used in production, it's not ready for that.

## Prerequisites

In order to execute the system correctly, just start the environment in a valid node (short & long names are allowed)

```shell
$ erl -pa _build/default/lib/*/ebin/ -name oox@127.0.0.1 -setcookie oox
```

After executing this all dependencies are set in path, just start them:

```erlang
(oox@127.0.0.1)1> application:start(syntax_tools).
ok
(oox@127.0.0.1)2> application:start(compiler).
ok
(oox@127.0.0.1)3> application:start(goldrush).
ok
(oox@127.0.0.1)4> application:start(lager).
ok
(oox@127.0.0.1)5> application:start(erlport).
ok
(oox@127.0.0.1)6> application:start(jun).
ok
(oox@127.0.0.1)7> application:start(oox).
ok
```

Don't worry about starting the apps in the environment if you use `oox` for example in a rebar.config,
this is only to show how the system works.

## Add a subscriber process:

Since jobs can be executed in async mode, the response will be delivered to a process, just set it:

```erlang
(oox@127.0.0.1)8> oox_scheduler:set_subscriber(self()).
ok
```

## Creating a new job

Now creates a new job, this will start a new slave on the host, ready to execute jun commands, it means that
slave contains an isolated jun environment, adding a job is easy:

```erlang
(oox@127.0.0.1)9> {ok, Job} = oox_scheduler:add_job("127.0.0.1").
{ok,<0.115.0>}
18:53:56.016 [info] ensuring started for jun main dependency, state ok
```

## Using commands

Define some commands in order to send & execute into slave via scheduler, the commands can be a specific overriden variables,
for example if in the command you want to use the worker, just use $worker in the args. Example:

```erlang
[[{mod,jun_pandas},
  {func,read_csv},
  {args,['$worker','./files/csv.txt']}],
 [{mod,jun_pandas},
  {func,head},
  {args,['$worker','$dataframe',1,[]]}],
 [{mod,jun_pandas},
  {func,max},
  {args,['$worker','$dataframe',no_age,[]]}]]
```

# Starting the job

Start the job to process the commands in the slave and waiting for response (remember response is delivered to the subscriber),
when job finish also the slave is stopped.

```erlang
(oox@127.0.0.1)10> oox_scheduler:start_job(Job, Cmds).
{ok,working}
```

# Receive response of job

In the subscriber process you will receive a response based on the execution of commands, there is two posibles responses: passed or broken.

```erlang
(oox@127.0.0.1)11> flush().
Shell got {job,<0.115.0>,{broken,{error,{'exceptions.KeyError',"Atom('fage')"}}}}
ok
```

#### Authors

@zgbjgg Jorge Garrido <zgbjgg@gmail.com>
