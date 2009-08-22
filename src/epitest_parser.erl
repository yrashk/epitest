-module(epitest_parser).
-export([parse_transform/2]).

-record(state,
	{ function }).

parse_transform(Forms, _Options) ->
    Tests = lists:flatten(forms(Forms, #state{})),
    Forms ++ [{function, 0, tests, 0, 
      [{clause, 0, [], [], [{string, 0, Tests}]}]
      }].


forms([{function, L, test, A, Cs}|Fs], St) ->
    [func(L, test, A,Cs,St)|forms(Fs,St)];
forms([_|Fs], St) ->
    [[]|forms(Fs,St)];
forms([], _St) ->
    [].


func(Line, Name, Arity, Clauses, St) ->
    State = St#state{ function = {Line, Name, Arity} },
    clauses(Clauses, State).

clauses([C|Cs],St) ->
    {clause, L,H0,_G,_B} = C,
    H1 = hd(heads(H0)),
    H = case lists:flatten(H1) of
	    H1 ->
		H1;
	    _ ->
		list_to_tuple(H1)
	end,
    [{L,H}|clauses(Cs,St)];
clauses([],_St) ->
    [].

heads([{_,_,H}|T]) ->
    [heads(H)|heads(T)];
heads([]) ->
    [];
heads(V) ->
    V.
