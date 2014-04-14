-module(other).
-compile([export_all]).

%% -spec run() -> any().
run() ->
    test:fib(25).

%% -spec id(any()) -> any().
id(X) ->
    X.

head([X]) ->
    X;
head([X | _]) ->
    X.

plus(X, Y) ->
    round(X + Y).
