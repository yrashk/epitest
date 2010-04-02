-module(epitest_sup).
-behaviour(supervisor).

-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok,{{one_for_one,0,1}, []}}.


