-module(epitest_helpers).
-export([pending/0, pending/1, fpending/0, fpending/1, fail/0, fail/1, ok/0,splitnodes/1, data/1, data/2]).

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
    lists:map(fun ({splitnode, Node}) ->
		      Node
	      end,
	      lists:filter(fun (Opt) ->
				   case Opt of
				       {splitnode, _} ->
					   true;
				       _ ->
					   false
				   end
			   end,			 
			   element(3, State))).

data(Name, Value) ->
    put(Name, Value).

data(Name) ->
    get(Name).
