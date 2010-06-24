-module(selftest).
-title("Self tests").

-include_lib("epitest/include/test.hrl").

test("Simple test with no dependencies") ->
    [
     ok()
    ];

test("Simple failing test") ->
    [fun () ->
             fail("It should fail, that's ok")
     end];

%% Parametrized tests

test({"Parametrized test", [Arg1, Arg2]}) ->
    [fun (State) ->
             pass(arg1, Arg1, State), 
             pass(arg2, Arg2, State)
     end];

test("Require parametrized test") ->
    [{require, [{success, [{"Parametrized test", [arg1, arg2]}]}]},
     fun(State) ->
             ?assertEqual(arg1, retr(arg1, State)),
             ?assertEqual(arg2, retr(arg2, State))
     end];


test("Require parametrized test with full module name") ->
    [{require, [{success, [{selftest, "Parametrized test", [arg1, arg2]}]}]},
     ok()];

%% Instantiation

test("Instantiable test") ->
    [instantiable,
     fun (State) ->
             pass(instantiated, signature(State), State)
     end];

test("Instantiation test #1") ->
    [{require, [{success, [instantiate("Instantiable test")]}]},
     fun (State) ->
             ?assertEqual(instantiate_signature("Instantiable test",loc(State)), retr(instantiated, State))
     end];

test("Instantiation test #2") ->
    [{require, [{success, [instantiate("Instantiable test")]}]},
     fun (State) ->
             ?assertEqual(instantiate_signature("Instantiable test",loc(State)), retr(instantiated, State))
     end];

%% Negative tests

test("Negative failing test should succeed") ->
    [negative,
     fun () ->
             fail()
     end];

test("Negative successful test should fail") ->
    [negative,
     ok()];

%% Time trapping

test("This test should fail because of the time trap") ->
    [
     {timetrap, {2000, milliseconds}},
     fun () ->
             timer:sleep(3000)
     end];

%% Skipping

test("Skipped test shoud not be executed") ->
    [skip,
     fun () ->
             fail("Should not be executed")
     end];

%% Dependencies

test("Simple test with a single dependency") ->
    [{require, [{success, ["Simple test with no dependencies"]}]},
     ok()];

test("Simple test that depends on a test with a dependency") ->
    [{require, [{success, ["Simple test with a single dependency"]}]},
     ok()];

test("Simple test with a single dependency references through a module") ->
    [{require, [{success, [{selftest, "Simple test with no dependencies"}]}]},
     ok()];

test("Simple test with a single dependency on a failing test") ->
    [{require, [{failure, ["Simple failing test"]}]},
     ok()];

test("Simple test with a non-existent dependency, should be unreachable") ->
    [{require, [{success, ["Non-existent test, should fail"]}]},
     ok()];

%% Passing variables

test("Pass data") ->
    [fun (State) ->
             pass(var1, "var1", State),
             pass(var2, "var2", State)
     end];

test("Retrieve passed data") ->
    [{require, [{success, ["Pass data"]}]},
     fun (State) ->
             ?assertEqual("var1", retr(var1, State)),
             ?assertEqual("var2", retr(var2, State))
     end];

%% Pending tests

test("Test with no functors should be pending") ->
    [];

test("Test specified as pending should be pending") ->
    [pending()];

test("Test specified as pending with custom description should be pending") ->
    [pending("Not Implemented")];

test("Test can make itself pending") ->
    [fun () ->
             make_pending()
     end];

test("Test can make itself pending with custom description") ->
    [fun () ->
             make_pending("Not Implemented")
     end];

%% Nodesplits

test("Capture master node") ->
    [fun (State) ->
             pass(master, node(), State)
     end];

test("Nodesplit should have functor started on another node") ->
    [nodesplit,
     {require, [{success, ["Capture master node"]}]},
     fun (State) ->
             Node = node(),
             ?assertNot(Node == retr(master, State))
     end];

test("Splitnode is inherited by dependant test") ->
    [{require, [{success, ["Nodesplit should have functor started on another node"]}]},
     fun (State) ->
             Node = node(),
             ?assertNot(Node == retr(master, State))
     end];
             

?EOT.
