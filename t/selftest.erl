-module(selftest).
-include_lib("epitest/include/test.hrl").

test("Simple test with no dependencies") ->
    [fun() -> 
             ok
     end];

test("Simple test with a single dependency") ->
    [{require, [{success, ["Simple test with no dependencies"]}]},
     fun () ->
             ok
     end];

?EOT.
