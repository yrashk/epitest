-module(epitest_parser).
-export([parse_transform/2]).

parse_transform(Forms0, _Options) ->
    Forms = forms(Forms0),
    Tests = list_to_conses(lists:map(fun ({L, FA}) ->
					     case is_tuple(FA) of
						 true ->
						     {tuple, 0, [{integer, 0, L},{tuple, 0, lists:map(fun (A) -> to_form(A,0) end, tuple_to_list(FA))}]};
						 false ->
						     {tuple, 0, lists:map(fun (A) -> to_form(A, 0) end, [0,FA])}
					     end
				     end, lists:flatten(test_forms(Forms))),0),
    Forms1 = lists:filter(fun (F) ->
				  case F of
				      {eof, _} ->
					  false;
				      _ ->
					  true
				  end
			  end, Forms),
    EndOfForms = Forms -- Forms1,
    Forms1 ++ [{function, 0, tests, 0, 
      [{clause, 0, [], [], [Tests]}]
      }] ++ EndOfForms.


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

clauses([{clause, L, [{tuple, _, T}] = H0,G,B}|Cs]) ->
    T1 = T ++ [{var, L, '_EpitestInstanceUniqueInstanceIdentifier'}],
    H1 = [{tuple, L, T1}],
    C0 = {clause, L, H0, G, [{match, L,
			      {var, L, '_EpitestInstanceUniqueInstanceIdentifier'},
			      {atom, L, main}} |B]},
    C1 = {clause, L, H1, G,B},
    [C0,C1|clauses(Cs)];
    
clauses([{clause, L, [{string, _, _}] = H0,G,B}|Cs]) ->
    H1 = [{tuple, L, [hd(H0), {var, L, '_EpitestInstanceUniqueInstanceIdentifier'}]}],
    C0 = {clause, L, H0, G, [{match, L,
			      {var, L, '_EpitestInstanceUniqueInstanceIdentifier'},
			      {atom, L, main}} |B]},
    C1 = {clause, L, H1, G,B},
    [C0,C1|clauses(Cs)];

clauses([C|Cs]) ->
    [C|clauses(Cs)];
clauses([]) ->
    [].


%
list_to_conses([], Line) ->
    {nil, Line};
list_to_conses([E|Rest], Line) ->
    {cons, Line, E, list_to_conses(Rest, Line)}.

to_form(S, Line) when is_list(S) ->
    {string, Line, S};
to_form(A, Line) when is_atom(A) ->
    {atom, Line, A};
to_form(I, Line) when is_integer(I) ->
    {integer, Line, I}.
