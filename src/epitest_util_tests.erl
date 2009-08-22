-module(epitest_util_tests).
-define(EPITEST_NOTRANSFORM, true).
-include_lib("epitest/include/epitest.hrl").
-export([tests/0,test/1]).

test({"All dependants", Mod, Name, Edge}) ->
    [
     {r, epitest:all_dependants({Mod, Name, []}, Edge)}
    ].

tests() ->
    [{0, {"All dependants",'Mod','Name','Edge'}}].
