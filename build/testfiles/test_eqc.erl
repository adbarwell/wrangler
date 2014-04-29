-module(test_eqc).

-compile([export_all]).

id(X) ->
    X.

plus(X, Y) ->
    X + Y.

skel() ->
    skel:do([{seq, fun ?MODULE:id/1}], [1,2,3]).
