-module(test_server_tests).
-include_lib("epitest/include/test.hrl").

test("Add test by title-only signature and descriptor") ->
    [fun() -> 
             Signature = "New test",
             Descriptor = [],
             {ok, Ref} = epitest_test_server:add(Signature, Descriptor),
             ?assertMatch(#test{loc = dynamic, signature = Signature},
                          epitest_test_server:lookup(Ref))
     end];

test("Add tests from a module") ->
    [fun(#epistate{ test = Test }) -> % TODO: finish this (it is incomplete)
             #test{ descriptor = _Descriptor } = Test,
             Module = ?MODULE,
             {ok, _Refs} = epitest_test_server:load(Module)
     end];

?EOT.
