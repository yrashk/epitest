-module(epitest_test_plan_sup).
-behaviour(supervisor).

-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    TestPlanSrv = {epitest_test_plan_server, {epitest_test_plan_server, start_link, []},
                   permanent, 5000, worker, dynamic},
    {ok,{{simple_one_for_one,0,1}, [TestPlanSrv]}}.
