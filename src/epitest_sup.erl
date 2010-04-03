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
    PropHandlerSup = {epitest_prophandler_sup, {epitest_prophandler_sup, start_link, []},
                      permanent, infinity, supervisor, dynamic},
    {ok,{{one_for_one,0,1}, [TestServer, PropHandlerSup]}}.


