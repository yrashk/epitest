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
    ];
      
test({"Parametrized forward dependency", Param}) ->
    [{f,
      fun (State) ->
	      "val" = ?GET(var, State),
	      "par1" = Param
      end}
     ].
      
	      
      
