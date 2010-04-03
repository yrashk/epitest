-module(test_server_tests).
-include_lib("epitest/include/test.hrl").

test("Add test by title-only signature and descriptor") ->
    [fun() -> 
             Signature = "New test",
             Descriptor = [],
             Ref = epitest_test_server:add(Signature, Descriptor),
             ?assertMatch(#test{loc = dynamic, signature = Signature, descriptor = Descriptor}, epitest_test_server:lookup(Ref))
     end];

test("Add tests from a module") ->
    [fun(#epistate{ descriptor = _Descriptor }) -> % TODO: finish this (it is incomplete)
             Module = ?MODULE,
             {ok, _Refs} = epitest_test_server:load(Module)
     end];

?EOT.
