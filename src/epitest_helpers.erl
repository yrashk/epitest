-module(epitest_helpers).
-export([pending/0, pending/1, fpending/0, fpending/1, fail/0, fail/1]).

pending() ->
    pending("PENDING IMPLEMENTATION").

pending(Reason) ->
    erlang:raise(error, {epitest_pending, Reason},[]).

fpending() ->
    fun pending/0.

fpending(Reason) ->
    fun () -> pending(Reason) end.
	    
fail() ->
    fail(noreason).
fail(Reason) ->
    erlang:raise(error, {failed, Reason}, []).
