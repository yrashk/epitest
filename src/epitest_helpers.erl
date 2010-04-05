-module(epitest_helpers).
-export([ok/0]).

ok() ->
    fun () ->
            ok
    end.

