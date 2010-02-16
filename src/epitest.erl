-module(epitest).
-vsn(?vsn).

-export([start/0,stop/0]).

-export([run/0, dependants/1, dependants/2, requires/1, requires/2, all_dependants/1, all_dependants/2, add_module/1, modules/1, status/1, remaining_tests/0, tests/0, test_descriptor/2]).


start() ->
    application:start(epitest).

stop() ->
    application:stop(epitest).


%%--------------------------------------------------------------------
%%% Public functions
%%--------------------------------------------------------------------

run() ->
    gen_server:cast(epitest_test_server, run).

dependants(Test) ->
    dependants(Test, '_').
dependants(Test, Label) ->
    gen_server:call(epitest_test_server, {dependants, Label, Test}).

requires(Test) ->
    requires(Test, '_').
requires(Test, Label) ->
    gen_server:call(epitest_test_server, {requires, Label, Test}).

all_dependants(Test) ->
    all_dependants(Test, '_').

all_dependants(Test, Label) ->
    gen_server:call(epitest_test_server, {all_dependants, Label, Test}).

add_module(Mod) ->
    gen_server:cast(epitest_test_server, {add_module, Mod}).

modules([Mod|T]) ->
    add_module(Mod),
    modules(T);
modules([]) ->
    ok.

status(Test) ->
    gen_server:call(epitest_test_server, {status, Test}).

remaining_tests() ->
    gen_server:call(epitest_test_server, remaining_tests).

tests() ->    
    gen_server:call(epitest_test_server, tests).


test_descriptor(M, [T]) when is_tuple(T) ->
    [Tn|A] = tuple_to_list(T),
    test_descriptor(M, Tn, A);

test_descriptor(M, [T]) ->
    test_descriptor(M, T, []).

test_descriptor(M, T, A) ->
    case ets:lookup(test_descriptors, {M, T, A}) of
        [] ->
            Saved = erase(),
            Args = 
                case A of
                    [] ->
                        [T];
                    _ ->
                        [list_to_tuple([T|A])]
                end,
            D = apply(M, test, Args),
            Dict = erase(),
            lists:foreach(fun ({K,V}) -> put(K,V) end, Saved),
            ets:insert(test_data, {{M, T, A}, Dict}),
            ets:insert(test_descriptors, {{M, A}, D}),
            D;
        [{_, D}] ->
            D
    end.

                      
