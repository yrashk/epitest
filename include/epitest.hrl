-ifndef(EPITEST_NOTRANSFORM).
-compile({parse_transform, epitest_parser}).
-export([test/1, tests/0]).
-import(epitest_helpers, [pending/0,pending/1,fpending/0,fpending/1]).
-endif.

 
-define(PASS(X), {epitest_variables, X}).
-define(GET(X, State), proplists:get_value(X, State#epistate.variables)).

-record(epistate,
	{
	  test,
	  options = [],
	  variables = [],
	  failure
	 }).


-define(all_dependants(X,E), {'CORE', "All dependants", [?MODULE, X, E]}).

-define(instantiate(Tuple0), 
	apply(fun (Tuple) ->
		      Instance = {?FILE,?LINE},
		      case Tuple of
			  T when is_list(T) ->
			      {T, [Instance]};
			  {M,T} when is_atom(M) ->
			      {M,T,[Instance]};
			  {T,A} ->
			      {T, A ++ [Instance]};
			  {M, T, A} ->
			      {M, T, A ++ [Instance]}
		      end
	      end, [Tuple0])).

