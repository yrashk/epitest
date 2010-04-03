-module(epitest_test_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    TestWorker = {epitest_test_worker, {epitest_test_worker, start_link, []},
                  transient, 5000, worker, dynamic},
    {ok,{{simple_one_for_one,0,1}, [TestWorker]}}.
