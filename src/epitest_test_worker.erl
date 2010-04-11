-module(epitest_test_worker).
-behavior(gen_fsm).

-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, booted/2, running/2, succeeded/2, failed/2,
         handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, { id, test_plan }).


start_link(Plan, Epistate) ->
    gen_fsm:start_link(?MODULE, [Plan, Epistate], []).

init([Plan, #epistate{ id = ID }]) ->
    {ok, booted, #state{ test_plan = Plan, id = ID }}.

booted(start, #state{ id = ID, test_plan = Plan } = State) ->
    Epistate = epitest_test_plan_server:lookup(Plan, ID),
    epitest_mod:handle_accum({start, Epistate}, epitest_test_server:lookup(ID)),
    {next_state, running, State}.

running(start, State) -> %% Ignore restarts if it is already running
    {next_state, running, State};

running(stop, State) ->
    {next_state, booted, State};
    
running(success, #state{ id = ID, test_plan = Plan } = State) ->
    gen_fsm:send_event(Plan, {success, ID}),
    {next_state, succeeded, State};

running({failure, Reason}, #state{ id = ID, test_plan = Plan } = State) ->
    gen_fsm:send_event(Plan, {failure, ID, Reason}),    
    {next_state, failed, State}.

%% Ignore events when it is over

succeeded(_, State) ->
    {next_state, succeeded, State}.

failed(_, State) ->
    {next_state, failed, State}.

%%

handle_event({notification, #epistate{} = NotificationEpistate}, StateName, #state{ id = ID, test_plan = Plan } = State) ->
    Epistate = epitest_test_plan_server:lookup(Plan, ID),
    epitest_mod:handle_accum({notification, Epistate, NotificationEpistate}, epitest_test_server:lookup(ID)),
    {next_state, StateName, State};

handle_event({update_epistate, Fun}, StateName, #state{ id = ID, test_plan = Plan } = State) ->
    epitest_test_plan_server:update_epistate(Plan, ID, Fun),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
