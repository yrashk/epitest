-module(epitest_test_worker).
-behavior(gen_fsm).

-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, booted/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, { id, test_plan }).


start_link(Plan, Epistate) ->
    gen_fsm:start_link(?MODULE, [Plan, Epistate], []).

init([Plan, #epistate{ id = ID }]) ->
		{ok, booted, #state{ test_plan = Plan, id = ID }}.

booted(start, #state{ id = ID } = State) ->
		epitest_prophandler:handle(start, epitest_test_server:lookup(ID)),
		{next_state, running, State}.

handle_event(_Event, StateName, State) ->
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
