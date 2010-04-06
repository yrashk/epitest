-module(selftest).
-title("Self tests").

-include_lib("epitest/include/test.hrl").

test("Simple test with no dependencies") ->
    [
     ok()
    ];

test("Simple test that should fail") ->
    [fun () ->
             fail("It should fail")
     end];

%% Parametrized tests

test({"Parametrized test", [Arg1, Arg2]}) ->
    [fun () ->
             Arg1, 
             Arg2
     end];

test("Require parametrized test") ->
    [{require, [{success, [{"Parametrized test", [arg1, arg2]}]}]},
     ok()];

%% Negative tests

test("Negative failing test should succeed") ->
    [negative,
     fun () ->
             fail()
     end];

test("Negative successful test should fail") ->
    [negative,
     ok()];

%% Dependencies

test("Simple test with a single dependency") ->
    [{require, [{success, ["Simple test with no dependencies"]}]},
     ok()];

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

?EOT.
