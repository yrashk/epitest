-module(epitest).
-include_lib("epitest/include/epitest.hrl").
-export([start/0,stop/0]).

%% Public helper
-export([remaining_tests/0, remaining_tests/1, test_state/1, test_state/2]).

start() ->
    application:start(epitest).

stop() ->
    application:stop(epitest).

%% Public helpers
remaining_tests() ->
    remaining_tests("Default").

remaining_tests(PlanName) ->
    case global:whereis_name({epitest_test_plan_server, epitest_cluster:name(), PlanName}) of
        undefined ->
            {plan_server_not_found, PlanName};
        Pid ->
            [ Test || #epistate{ test = Test } <- epitest_test_plan_server:remaining(Pid) ]
    end.

test_state(#test{} = Test) ->
    test_state(Test, "Default").

test_state(#test{ id = ID }, PlanName) ->
    case global:whereis_name({epitest_test_plan_server, epitest_cluster:name(), PlanName}) of
        undefined ->
            {plan_server_not_found, PlanName};
        Pid ->
            epitest_test_plan_server:lookup(Pid, ID)
    end.

    
