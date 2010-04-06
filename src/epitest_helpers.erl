-module(epitest_helpers).
-export([ok/0, pending/0, pending/1, make_pending/0, make_pending/1, fail/0, fail/1]).

ok() ->
    fun () ->
            ok
    end.

pending() ->
    pending("PENDING IMPLEMENTATION").

pending(Reason) ->
    fun () ->
            epitest_helpers:make_pending(Reason)
    end.

make_pending() ->
    make_pending("PENDING IMPLEMENTATION").

make_pending(Reason) ->
    erlang:error({pending, Reason}).

fail() ->
    fail(noreason).

fail(Reason) ->
    erlang:error(Reason).
