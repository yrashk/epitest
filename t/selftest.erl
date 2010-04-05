-module(selftest).
-title("Self tests").

-include_lib("epitest/include/test.hrl").

test("Simple test with no dependencies") ->
    [
     ok()
    ];

test("Simple test with a single dependency") ->
    [{require, [{success, ["Simple test with no dependencies"]}]},
     ok()];

?EOT.
