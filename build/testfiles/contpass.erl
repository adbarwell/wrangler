-module(contpass).

-compile([export_all]).

id() ->
    ok.

id(X, K) ->
    K(X).

succ(X, K) ->
    K(X + 1).

print(X, K) ->
    io:format("X: ~p~n", [X]),
    K().

plus(X, Y) ->
    X + Y.

cplus(X, Y, K) ->
    K(X + Y).
