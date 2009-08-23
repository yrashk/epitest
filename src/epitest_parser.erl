-module(epitest_parser).
-export([parse_transform/2]).

parse_transform(Forms0, _Options) ->
    Forms = forms(Forms0),
    Tests = lists:flatten(test_forms(Forms)),
    Forms ++ [{function, 0, tests, 0, 
      [{clause, 0, [], [], [{string, 0, Tests}]}]
      }].


test_forms([{function, L, test, A, Cs}|Fs]) ->
    [test_func(L, test, A,Cs)|test_forms(Fs)];
test_forms([_|Fs]) ->
    [[]|test_forms(Fs)];
test_forms([]) ->
    [].


test_func(_Line, _Name, _Arity, Clauses) ->
    test_clauses(Clauses).

test_clauses([C|Cs]) ->
    {clause, L,H0,_G,_B} = C,
    H1 = hd(test_heads(H0)),
    H = case lists:flatten(H1) of
	    H1 ->
		H1;
	    _ ->
		list_to_tuple(H1)
	end,
    [{L,H}|test_clauses(Cs)];
test_clauses([]) ->
    [].

test_heads([{_,_,H}|T]) ->
    [test_heads(H)|test_heads(T)];
test_heads([]) ->
    [];
test_heads(V) ->
    V.





%
forms([{function, L, test, 1, Cs}|Fs]) ->
    [func(L, test, 1,Cs)|forms(Fs)];
forms([F|Fs]) ->
    [F|forms(Fs)];
forms([]) ->
    [].


func(Line, Name, Arity, Clauses) ->
    {function, Line, Name, Arity, clauses(Clauses)}.

clauses([{clause, L, [{string, _, _}] = H0,G,B}=C|Cs]) ->
    H1 = [{tuple, L, [hd(H0), {var, L, '_Instance'}]}],
    C1 = {clause, L, H1, G,B},
    [C,C1|clauses(Cs)];

clauses([C|Cs]) ->
    [C|clauses(Cs)];
clauses([]) ->
    [].
