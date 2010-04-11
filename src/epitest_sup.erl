-module(epitest_sup).
-behaviour(supervisor).

-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    TestServer = {epitest_test_server, {epitest_test_server, start_link, []},
                  permanent,2000,worker,[epitest_test_server]},
    ModsSup = {epitest_mods_sup, {epitest_mods_sup, start_link, []},
                      permanent, infinity, supervisor, dynamic},
    TestPlanSup = {epitest_test_plan_sup, {epitest_test_plan_sup, start_link, []},
                   permanent, infinity, supervisor, dynamic},
    TestWorkerSup = {epitest_test_worker_sup, {epitest_test_worker_sup, start_link, []},
                    permanent, infinity, supervisor, dynamic},
    SlaveServer = {epitest_slave_server, {epitest_slave_server, start_link, []},
                   permanent, 5000, worker, [epitest_slave_server]},
    {ok,{{one_for_one,0,1}, [TestServer, ModsSup, TestPlanSup, TestWorkerSup, SlaveServer]}}.


