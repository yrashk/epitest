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
	case Tuple of
	    T when is_list(T) ->
		{T, [make_ref()]};
	    {T,A} ->
		{T, A++[make_ref()]};
	    {M, T, A} ->
		{M, T, A ++[make_ref()]}
	end).

-define(instantiable(T), {T, _Instance}).
