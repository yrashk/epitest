-ifndef(EPITEST_NOTRANSFORM).
-compile({parse_transform, epitest_parser}).
-export([test/1, tests/0]).
-endif.

 
-define(PASS(X), {epitest_variables, X}).
-define(GET(X, State), proplists:get_value(X, State#epistate.variables)).

-record(epistate,
	{
	  test,
	  variables = []
	 }).


-define(all_dependants(X,E), {'CORE', "All dependants", [?MODULE, X, E]}).
