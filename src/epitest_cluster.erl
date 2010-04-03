-module(epitest_cluster).
-export([name/0]).

name() ->
    proplists:get_value(cluster_name, application:get_all_env(epitest), default).
