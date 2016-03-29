doppler
=======

Simple moking library for Erlang. The key features are:

* Orientation to DI-specific code. I.e. *it does not allow* to stub arbitrary module calls (`foo:bar(baz)`) like `em`, `meck` etc. See example for details.
* Simplicity. It does not use any complicated features like module code reload, etc.
* Preserving state between calls of mock's methods. This allows to easily mock `gen_server` interfaces.

Build
-----

    $ make

Tests
-----

    $ make test

Sample usage
------------

Consider the following example. We need to compose a function which prints current time to stdout. A common implementation is:

```erlang
print_time() ->
    Time = erlang:system_time(seconds),
    io:format("~p", [Time])
```

How could we test it? The possible way is to "mock" `erlang:system_time` and `io:format` calls. But if we then change `io:format` call to `io:fwrite` or use `os:timestamt` instead of `erlang:system_time`, our code will still work, but tests will fail. This generally shouldn't happen: if a function's interface does not change, the tests shouldn't fail. This points out that our function's contract is wider then its interface: it implicitly uses "global" functions. Such code id difficult to test and support.

One way to improve situation is to make contract "explicit".


```erlang

-module(system).
-export([time/0, print/1]).

time() ->
    erlang:system_time(seconds).

print(Str) ->
    io:format(Str).
```


```erlang
print_time(System) ->
    Time = System:time(),
    System:print(io_lib:format("~p", [Time])).
```

Now we are able to write tests which do not break as long as our function keeps it's interface.

```erlang
print_time_test(_Config) ->
    PredefinedTime = 1459246205,
    System = doppler:start(undefined),
    doppler:def(System, time, fun(State) ->  {PredefinedTime, State} end),
    doppler:def(System, print, fun(_State, Str) ->
        NewState = Str,
        {ok, NewState}
    end),

    print_time(System),

    PrintedTime = doppler:state(),
    doppler:stop(System).

    PrintedTime = "1459246205".

```

Here we use `doppler` to mock `system` module and keep printed time in our doppler's state. We also can view the whole history of calls to our module:

```erlang
> doppler:log(System).
[{time,[]},{print,[["1459246205"]]}]
```

Of course, one can write a mock for `System` from scratch as a plain module, but this is a tedious job: we need to have a seperate module for each mock used, and also we should write a lot of boilerplate code for passing state between mocked calls.





