-include_lib("epitest/include/epitest.hrl").
-spec test(test_signature()) -> test_descriptor().

%% Autoexport tests
-export([test/1]).

%% End Of Tests marker
-define(EOT, test("EOT") -> [skip]).
