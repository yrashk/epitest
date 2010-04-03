-module(epitest_test_plan_server).
-behaviour(gen_fsm).
-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER(Name), {?MODULE, epitest_cluster:name(), Name}).

-record(state, {
          epistates
         }).

start_link(Name, PlanFun) ->
    gen_fsm:start_link({global, ?SERVER(Name)}, ?MODULE, epitest_test_server:q(PlanFun), []).

init(Tests) ->
    Epistates = [ #epistate{ id = ID, test = Test } || (#test{ id = ID } = Test) <- Tests ],
    EpistatesTab = ets:new(epitest_states, [public, {keypos, 2}]),
    ets:insert(EpistatesTab, Epistates),
    {ok, initialized, #state{
           epistates = EpistatesTab
          }}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

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



