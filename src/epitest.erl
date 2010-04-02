-module(epitest).

-export([start/0,stop/0]).

start() ->
    application:start(epitest).

stop() ->
    application:stop(epitest).
