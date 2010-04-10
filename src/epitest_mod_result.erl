-module(epitest_mod_result).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({{start,  #epistate{ id = ID, test_plan = Plan, worker = Worker }},
             #test{} = Test}, _From, State) ->
    #epistate{ mods_properties = Properties } = epitest_test_plan_server:lookup(Plan, ID),
    Result = proplists:get_value(functor_result, Properties),
    gen_fsm:send_event(Worker, Result),
    {reply, {ok, Test}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.
