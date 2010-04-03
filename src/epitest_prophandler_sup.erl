-module(epitest_prophandler_sup).
-behaviour(supervisor).

-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Sup = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    init_handlers(),
    Sup.

init([]) ->
    PropHandler = {epitest_prophandler, {epitest_prophandler, start_link, []},
                   permanent, 5000, worker, dynamic},
    {ok,{{simple_one_for_one,0,1}, [PropHandler]}}.


%%% Internal functions
init_handlers() ->
    Handlers = proplists:get_value(property_handlers, application:get_all_env(epitest), []),
    [ init_handler(Handler) || Handler <- Handlers ].

init_handler(Handler) ->
    supervisor:start_child(?SERVER, [Handler]).
