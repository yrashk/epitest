-module(epitest_helpers).
-export([pending/0, pending/1, fpending/0, fpending/1]).

pending() ->
    pending("PENDING IMPLEMENTATION").

pending(Reason) ->
    throw({epitest_pending, Reason}).

fpending() ->
    fun pending/0.

fpending(Reason) ->
    fun () -> pending(Reason) end.
	    
