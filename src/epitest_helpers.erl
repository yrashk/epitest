-module(epitest_helpers).
-define(NO_AUTOIMPORT, 1).
-include_lib("epitest/include/epitest.hrl").
-export([ok/0, pending/0, pending/1, make_pending/0, make_pending/1, fail/0, fail/1, pass/3, retr/2]).

ok() ->
    fun () ->
            ok
    end.

pending() ->
    pending("PENDING IMPLEMENTATION").

pending(Reason) ->
    fun () ->
            epitest_helpers:make_pending(Reason)
    end.

make_pending() ->
    make_pending("PENDING IMPLEMENTATION").

make_pending(Reason) ->
    erlang:error({pending, Reason}).

fail() ->
    fail(noreason).

fail(Reason) ->
    erlang:error(Reason).

pass(Name, Value, #epistate{ id = ID, test_plan = Plan }) ->
    epitest_test_plan_server:update_epistate(Plan, ID, 
                                             fun (#epistate{ variables = Variables } = Epistate) ->
                                                     Epistate#epistate{ variables = [{Name, Value}|Variables] }
                                             end),
    ok.

retr(Name, #epistate{ id = ID, test_plan = Plan }) ->
    #epistate{ variables = Variables } = epitest_test_plan_server:lookup(Plan, ID),
    proplists:get_value(Name, Variables).
