-module(epitest_sup).
-vsn(?vsn).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    ets:new(test_descriptors, [public, set, named_table]),
    ets:new(test_data, [public, set, named_table]),
    TestServer = {epitest_test_server, {epitest_test_server, start_link, []},
		  permanent,2000,worker,[epitest_test_server]},
    TestSup = {epitest_test_sup,  {epitest_test_sup, start_link, []},
		  permanent,infinity,supervisor,[epitest_test_sup]},
    TestLogMgr = {epitest_log, {gen_event, start_link, [{local, epitest_log}]},
		  permanent, 2000, worker, [gen_event]},
    FileServer = {epitest_file_server, {epitest_file_server, start_link, []},
		  permanent, 2000, worker, [epitest_file_server]},
    SlaveServer = {epitest_slave, {epitest_slave, start_server_link, []},
		  permanent, 2000, worker, [epitest_slave]},
    {ok,{{one_for_one,0,1}, [TestServer, TestSup, TestLogMgr,FileServer,SlaveServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
