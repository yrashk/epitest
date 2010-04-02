-module(epitest_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    epitest_sup:start_link(StartArgs).

stop(_State) ->            
    ok.
