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
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Another simple test that fails") ->
    [{fmsg, "It should have failed, that's all right"},
     {f,
      fun () ->
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Another simple test that fails (with State)") ->
    [{fmsg, "It should have failed, that's all right"},
     {f,
      fun (_State) ->
	      5 = apply(fun erlang:'+'/2, [2,2])
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
	      4 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Simple test with a single r dependency that should never be reached") ->
    [
     {r, ["Simple test that fails"]},
     {f,
      fun () ->
	      io:format("[YOU REACHED TEST THAT YOU SHOULDN'T #1]"),
	      ok
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
	      io:format("[YOU REACHED TEST THAT YOU SHOULDN'T #3]")
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
	      io:format("[YOU REACHED TEST THAT YOU SHOULDN'T #3]")
      end}
    ];


test("Another simple test with a multiple fr dependency that should never be reached") ->
    [
     {fr, ["Simple test with no dependencies, no function","Simple test with no dependencies"]},
     {f,
      fun () ->
	      io:format("[YOU REACHED TEST THAT YOU SHOULDN'T #4]")
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

test("Pass the node") ->
    [
     {f,
      fun () ->
	      ?PASS([{node,node()}])
      end}
     ];
      
test("Split node test") -> 
    [
     nodesplit,
     {r, ["Pass the node"]},
     {f,
      fun (State) ->
	      false = node() == ?GET(node, State),
	      true = proplists:get_value(splitnode, State#epistate.options) == node()
      end}];

test("Negative split node test") -> 
    [
     negative,
     nodesplit,
     {r, ["Pass the node"]},
     {f,
      fun (State) ->
	      true = node() == ?GET(node, State)
      end}].
