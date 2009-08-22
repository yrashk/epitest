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
    [{f,
      fun () ->
	      io:format("[next failure is ok]"),
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Another simple test that fails") ->
    [{f,
      fun () ->
	      io:format("[next failure is ok]"),
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Simple negative test") ->
    [negative,
     {f,
      fun () ->
	      5 = apply(fun erlang:'+'/2, [2,2])
      end}];

test("Simple negative test that fails") ->
    [negative,
     {f,
      fun () ->
	      io:format("[next failure is ok]"),
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

test("Split node test") ->
    [
     nodesplit,
     {f,
      fun () ->
	      io:format("[Nodesplit: ~p]",[node()])
      end}].
