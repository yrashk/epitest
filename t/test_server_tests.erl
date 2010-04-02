-module(test_server_tests).
-include_lib("epitest/include/test.hrl").

test("Add test by title-only signature and descriptor") ->
		[fun() -> 
						 Signature = "New test",
						 Descriptor = [],
						 epitest_test_server:add(Signature, Descriptor),
						 ?assertEqual(Descriptor, epitest_test_server:lookup(Signature))
		 end];

test("Add tests from a module") ->
		[fun(#epistate{ descriptor = Descriptor }) -> 
						 Module = ?MODULE,
						 epitest_test_server:load(Module),
						 ?assertEqual(Descriptor, epitest_test_server:lookup("Add tests from a module"))
		 end];

?EOT.
