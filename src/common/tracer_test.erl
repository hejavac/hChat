
-module(tracer_test).

-include_lib("stdlib/include/ms_transform.hrl").
-compile(export_all).

%% http://erlang.org/doc/man/dbg.html

%% dbg
test1() ->
    dbg:tracer(),
    dbg:tpl(tracer_test, fib, '_', dbg:fun2ms(fun(_) -> return_trace() end)),
    dbg:p(all, [c]),
    tracer_test:fib(4).

test2() ->
    trace_module(tracer_test, fun() -> fib(4) end).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

trace_module(Mod, StartFun) ->
    spawn(fun() -> trace_module1(Mod, StartFun) end).

trace_module1(Mod, StartFun) ->
    % http://www.cnblogs.com/me-sa/archive/2011/10/22/erlang0008.html
    erlang:trace_pattern(
        {Mod, '_', '_'},
        [{'_', [], [{return_trace}]}],
        [local]
        ),
    S = self(),
    Pid = spawn(fun() -> do_trace(S, StartFun) end),
    erlang:trace(Pid, true, [call, procs]),
    Pid ! {self(), start},
    trace_loop().

do_trace(Parent, StartFun) ->
    receive
        {Parent, start} ->
            StartFun()
    end.

trace_loop() ->
    receive
        {trace, _, call, X} ->
            io:format("Call: ~p~n", [X]),
            trace_loop();
        {trace, _, return_from, Call, Ret} ->
            io:format("Return From: ~p => ~p~n", [Call, Ret]),
            trace_loop();
        Other ->
            io:format("Other = ~p~n", [Other]),
            trace_loop()
    end.

%% 堆栈输出
back_trace() ->
    F = fun({M, F, ArgNum, [{_File, _FileName}, {_Line, Line}]}, N) -> 
            io:format("~s~p:~p/~p [line:~p]~n", [lists:append(lists:duplicate(N, " ")), M, F, ArgNum, Line]), 
            N + 2 
        end,
    lists:foldl(F, 0, try throw(42) catch 42 -> erlang:get_stacktrace() end).