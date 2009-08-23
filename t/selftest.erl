-module(selftest).
-include_lib("epitest/include/epitest.hrl").

test("Simple test with no dependencies, no function") ->
    [];

test("Simple test with no dependencies") ->
    [
     {f,
      fun () ->
	      ok
      end}
    ];

test("Simple test with a single r dependency") ->
    [
     {r, ["Simple test with no dependencies"]},
     {f,
      fun () ->
	      ok
      end}
    ];

test("Simple test that fails") ->
    [{fmsg, "It should have failed, that's all right"},
     {f,
      fun () ->
	      throw(ok)
      end}];

test("Another simple test that fails") ->
    [{fmsg, "It should have failed, that's all right"},
     {f,
      fun () ->
	      throw(ok)
      end}];

test("Another simple test that fails (with State)") ->
    [{fmsg, "It should have failed, that's all right"},
     {f,
      fun (_State) ->
	      throw(ok)
      end}];

test("Simple negative test") ->
    [negative,
     {f,
      fun () ->
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Simple negative test that fails") ->
    [{fmsg, "It should have failed, that's all right"},
     negative,
     {f,
      fun () ->
	      ok
      end}];

test("Simple test with a single r dependency that should never be reached") ->
    [
     {r, ["Simple test that fails"]},
     {f,
      fun () ->
	     throw("it shouldn't have been reached")
      end}
    ];

test("Simple test with a single fr dependency") ->
    [
     {fr, ["Simple test that fails"]},
     {f,
      fun () ->
	      ok
      end}
    ];

test("Simple test with a single ir dependency") ->
    [
     {ir, ["Simple test that fails"]},
     {f,
      fun () ->
	      ok
      end}
    ];

test("Another simple test with a single ir dependency") ->
    [
     {ir, ["Simple test with no dependencies, no function"]},
     {f,
      fun () ->
	      ok
      end}
    ];

test("Simple test with a multiple r dependency") ->
    [
     {r, ["Simple test with no dependencies, no function","Simple test with no dependencies"]},
     {f,
      fun () ->
	      ok
      end}
    ];

    
test("Simple test with a multiple r dependency that should never be reached") ->
    [
     {r, ["Simple test that fails","Simple test with no dependencies"]},
     {f,
      fun () ->
	     throw("it shouldn't have been reached")
      end}
    ];

test("Testing for different variables coming from multiple dependencies") ->
    [
     {r, ["Pass the variable","Pass the node"]},
     {f,
      fun (State) ->
	      Node = node(),
	      Node = ?GET(node, State),
	      "val" = ?GET(var, State)
      end}
    ];


test("Simple test with a multiple fr dependency") ->
    [
     {fr, ["Simple test that fails","Another simple test that fails"]},
     {f,
      fun () ->
	      ok
      end}
    ];
    

test("Simple test with a multiple fr dependency that should never be reached") ->
    [
     {fr, ["Simple test that fails","Simple test with no dependencies"]},
     {f,
      fun () ->
	     throw("it shouldn't have been reached")
      end}
    ];


test("Another simple test with a multiple fr dependency that should never be reached") ->
    [
     {fr, ["Simple test with no dependencies, no function","Simple test with no dependencies"]},
     {f,
      fun () ->
	     throw("it shouldn't have been reached")
      end}
    ];

test("Simple test with a multiple ir dependency") ->
    [
     {ir, ["Simple test that fails", "Simple test with no dependencies, no function","Simple test with no dependencies"]},
     {f,
      fun () ->
	      ok
      end}
    ];

test("f/1") ->
    [
     {f,
      fun (S) ->
	      true = is_record(S, epistate)
      end}
     ];


test("Pass the variable") ->
    [
     {f,
      fun () ->
	      ?PASS([{var, "val"}])
      end}
     ];

test("Check the variable") ->
    [
     {r, ["Pass the variable"]},
     {f,
      fun (State) ->
	      "val" = ?GET(var, State)
      end}
     ];

test("Alter the variable") ->
    [
     {r, ["Pass the variable"]},
     {f,
      fun () ->
	       ?PASS([{var, "val1"}])
      end}
     ];

test("Check the variable after alteration") ->
    [
     {r, ["Alter the variable"]},
     {f,
      fun (State) ->
	      "val1" = ?GET(var, State)
      end}
     ];

test("Skipped test") -> 
    [
     skip,
     {f,
      fun () ->
	      throw("This test should have been skipped!")
      end}
     ];

test("Instantiable test") -> % makes use of skip!
    [
     skip,
     {f,
      fun () ->
	      ok
      end}
     ];

test("Instantiable test requirement test") ->
    [
     {r, [?instantiate("Instantiable test")]},
     {f,
      fun () ->
	      ok
      end}
     ];

test("Multiple instantiable test requirement test") ->
    [
     {r, [?instantiate("Instantiable test"), ?instantiate({anothertest, "Instantiable"})]},
     {f,
      fun () ->
	      ok
      end}
     ];



test("Pass the node") ->
    [
     {f,
      fun () ->
	      ?PASS([{node,node()},{"Pass the node", yes}])
      end}
     ];
      
test("Split node test") -> 
    [
     nodesplit,
     {r, ["Pass the node"]},
     {f,
      fun (State) ->
	      false = node() == ?GET(node, State),
	      true = proplists:get_value(splitnode, State#epistate.options) == node(),
	      ?PASS([{splitted, node()},{"Split node test", yes}])
      end}];

test("Split node continuation test") -> 
    [
     {r, ["Split node test"]},
     {f,
      fun (State) ->
	      Node = node(),
	      Node = proplists:get_value(splitnode, State#epistate.options),
	      ?PASS([{"Split node continuation test",yes}])
      end}];

test("Split node split test") -> 
    [
     nodesplit,
     {r, ["Split node test"]},
     {f,
      fun (State) ->
	      Node = node(),
	      false = Node == ?GET(splitted, State),
	      ?PASS([{"Split node split test",yes}])
      end}];

test("Negative split node test") -> 
    [
     negative,
     nodesplit,
     {r, ["Pass the node"]},
     {f,
      fun (State) ->
	      true = node() == ?GET(node, State)
      end}];


test("Split node with arguments test") -> 
    [
     {nodesplit, [{args,"-kernel testflag yes"}]},
     {f,
      fun () ->
	      {ok, yes} = application:get_env(kernel, testflag)
      end}];


test("All dependants test") ->
    [{r, [?all_dependants("Pass the node",r)]},
     {f,
      fun (State) ->
	      yes = ?GET("Pass the node", State),
	      yes = ?GET("Split node test", State),
	      yes = ?GET("Split node continuation test", State),
	      yes = ?GET("Split node split test", State)
	      % we can't check "Negative split node test" for now, because it is negative
      end
      }];

test("Repeat until test") -> % TODO: make a better test
    [{repeat, {until, 100}},
     {f,
      fun () ->
	      case get(counter) of
		  undefined ->
		      put(counter, 1);
		  N ->
		      put(counter, N+1)
	      end,
	      get(counter)
      end}
     ];

test("Repeat while test") -> % TODO: make a better test
    [{repeat, {while, 1}},
     {f,
      fun () ->
	      case get(counter) of
		  undefined ->
		      put(counter, 1);
		  N ->
		      put(counter, N+1)
	      end,
	      get(counter)
      end}
     ];

test("Another module's test dependency") ->
    [{r,[{anothertest,"Just some test"}]}];
test("Another module's test dependency #2") ->
    [{r,[{anothertest,"Just some test #2", []}]}];
test("Another module's instantiable dependency") ->
    [{r, [?instantiate({anothertest, "Instantiable"})]}];
   
test("Some test with forward dependencies") ->
    [{d, ["Some forward dependency",
	  {anothertest, "Some forward dependency"},
	  {"Parametrized forward dependency",["par1"]},
	  {anothertest, "Parametrized forward dependency",["par1"]}
	 ]},
     {f,
      fun () ->
	      ?PASS([{var, "val"}])
      end}
     ];
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
      
	      
      
