-ifndef(EPITEST_NOTRANSFORM).
-compile({parse_transform, epitest_parser}).
-export([test/1, tests/0]).
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

-define(instantiate(Tuple), 
	case {{?FILE,?LINE}, Tuple} of
	    {Instance,T} when is_list(T) ->
		{T, [Instance]};
	    {Instance, {M,T}} when is_atom(M) ->
		{M,T,[Instance]};
	    {Instance,{T,A}} ->
		{T, A ++ [Instance]};
	    {Instance,{M, T, A}} ->
		{M, T, A ++ [Instance]}
	end).

