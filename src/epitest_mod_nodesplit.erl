-module(epitest_mod_nodesplit).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

%% handle_call({normalize, #test{} = Test}, _From, State) ->
%%     {reply, {ok, Test}, State};

%% handle_call({{start, #epistate{ id = ID, test_plan = Plan } = Epistate}, #test{} = Test}, From, State) ->
%%     {noreply, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.
