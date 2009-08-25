-module(epitest_helpers).
-export([pending/0, pending/1, fpending/0, fpending/1, fail/0, fail/1, ok/0,splitnodes/1]).

pending() ->
    pending("PENDING IMPLEMENTATION").

pending(Reason) ->
    erlang:raise(error, {epitest_pending, Reason},erlang:get_stacktrace()).

fpending() ->
    fun pending/0.

fpending(Reason) ->
    fun () -> pending(Reason) end.
	    
fail() ->
    fail(noreason).
fail(Reason) ->
    erlang:raise(error, {failed, Reason},erlang:get_stacktrace()).

ok() ->
    fun () ->
	    ok
    end.

splitnodes(State) ->
    lists:filter(fun (Opt) ->
			 case Opt of
			     {splitnode, _} ->
				 true;
			     _ ->
				 false
			 end
		 end,			 
		 element(2, State)).
