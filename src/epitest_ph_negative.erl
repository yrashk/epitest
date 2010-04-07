-module(epitest_ph_negative).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({{start,  #epistate{ id = ID, test_plan = Plan, worker = Worker }},
             #test{ descriptor = Descriptor } = Test}, _From, State) ->
    #epistate{ handlers_properties = Properties } = epitest_test_plan_server:lookup(Plan, ID),
    Invert = proplists:get_value(negative, Descriptor, false),
    Result = proplists:get_value(functor_result, Properties),
    gen_fsm:send_event(Worker, result(Invert, Result)),
    {reply, {ok, Test}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.


result(true, success) ->
    {failure, negative};
result(true, {failure, _}) ->
    success;
result(false, Result) ->
    Result.
