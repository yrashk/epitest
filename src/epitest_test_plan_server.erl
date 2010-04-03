-module(epitest_test_plan_server).
-behaviour(gen_fsm).
-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, booted/2, ready/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([lookup/2]).

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
    gen_fsm:send_event(self(), initialize),
    {ok, booted, #state{
           epistates = EpistatesTab
          }}.

booted(initialize, State) ->
    initialize_workers(State),
    {next_state, ready, State}.

ready(start, State) ->
    spawn(fun () -> start_workers(State) end),
    {next_state, running, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event({lookup, ID}, _From, StateName, #state{ epistates = Epistates } = State) ->
    Reply =
        case ets:lookup(Epistates, ID) of
            [] -> {error, notfound};
            [Epistate] ->
                Epistate
        end,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Public function
-spec lookup(pid(), test_id()) -> #epistate{} | {'error', any()}.

lookup(Server, ID) ->
    gen_fsm:sync_send_all_state_event(Server, {lookup, ID}).

%% Internal function

initialize_workers(#state{ epistates = Epistates }) ->
    EpistateList = ets:tab2list(Epistates),
    lists:foreach(fun (Epistate) ->
                          {ok, Pid} = supervisor:start_child(epitest_test_worker_sup, [self(), Epistate]),
                          link(Pid),
                          ets:insert(Epistates, Epistate#epistate{ pid = Pid })
                  end, EpistateList).

start_workers(#state{ epistates = Epistates }) ->
    EpistateList = ets:tab2list(Epistates),
    [ gen_fsm:send_event(Pid, start) || #epistate{ pid = Pid } <- EpistateList ].
