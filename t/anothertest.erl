-module(anothertest).
-include_lib("epitest/include/epitest.hrl").

test("Just some test") ->
    [];
test("Just some test #2") ->
    [];
test("Instantiable") ->
    [];
test("Some forward dependency") ->
    [{f,
      fun (State) ->
	      "val" = ?GET(var, State)
      end}
    ].
	      
      
	      
      
