-module(epitest_mods_sup).
-behaviour(supervisor).

-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Sup = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    init_mods(),
    Sup.

init([]) ->
    Mod = {epitest_mod, {epitest_mod, start_link, []},
                   permanent, 5000, worker, dynamic},
    {ok,{{simple_one_for_one,0,1}, [Mod]}}.


%%% Internal functions
init_mods() ->
    Mods = proplists:get_value(mods, application:get_all_env(epitest), []),
    [ init_mod(Mod) || Mod <- Mods ].

init_mod({Mod, Properties}) when is_atom(Mod) ->
    supervisor:start_child(?SERVER, [Mod, Properties]);
init_mod(Mod) when is_atom(Mod) ->
    init_mod({Mod, []}).
