-module(epitest_test_plan_server).
-behaviour(gen_fsm).
-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, booted/2, running/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([lookup/2, update_epistate/3]).

-define(SERVER(Name), {?MODULE, epitest_cluster:name(), Name}).

-record(state, {
          epistates,
          event_mgr
         }).

start_link(Name, PlanFun) ->
    gen_fsm:start_link({global, ?SERVER(Name)}, ?MODULE, epitest_test_server:q(PlanFun), []).

init(Tests) ->
    EpistatesTab = ets:new(epitest_states, [public, {keypos, 2}]),

    State = #state{
           epistates = EpistatesTab,
           event_mgr = initialize_event_mgr()
          },

    load_tests(Tests, self(), State),

    gen_fsm:send_event(self(), run),

    {ok, booted, State}.

booted({load, Tests}, State) ->
    load_tests(Tests, self(), State),
    {next_state, booted, State};

booted(run, State) ->
    initialize_workers(State),
    spawn(fun () -> start_workers(State) end),
    {next_state, running, State}.
    
running({success, ID}, #state{ event_mgr = EventMgr, epistates = Epistates } = State) ->
    Epistate0 = do_lookup(ID, Epistates),
    Epistate = Epistate0#epistate{ state = succeeded },
    ets:insert(Epistates, Epistate),
    gen_event:notify(EventMgr, Epistate),
    process_remaining_tests(State);

running({failure, ID, Reason}, #state{ event_mgr = EventMgr, epistates = Epistates } = State) ->
    Epistate0 = do_lookup(ID, Epistates),
    Epistate = Epistate0#epistate{ state = {failed, Reason} },
    ets:insert(Epistates, Epistate),
    gen_event:notify(EventMgr, Epistate),
    process_remaining_tests(State).

handle_event({update_epistate, ID, Fun}, StateName, #state{ epistates = Epistates } = State) ->
    case ets:lookup(Epistates, ID) of
        [] ->
            ignore;
        [Epistate0] ->
            Epistate = Fun(Epistate0),
            ets:insert(Epistates, Epistate)
    end,
    {next_state, StateName, State}.

handle_sync_event({lookup, ID}, _From, StateName, #state{ epistates = Epistates } = State) ->
    Reply = do_lookup(ID, Epistates),
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

-spec update_epistate(pid(), test_id(), fun((#epistate{}) -> #epistate{})) -> 'ok'.

update_epistate(Server, ID, Fun) ->
    gen_fsm:send_all_state_event(Server, {update_epistate, ID, Fun}).
                               

%% Internal function

initialize_event_mgr() ->
    {ok, EventMgr} = gen_event:start_link(),
    [ gen_event:add_handler(EventMgr, Handler, []) || Handler <- proplists:get_value(test_plan_handlers, application:get_all_env(epitest), []) ],
    EventMgr.


load_tests(Tests, Plan, #state{ epistates = EpistatesTab }) ->
    Epistates0 = lists:map(fun (#test{ id = ID } = Test0) ->
                                   Test = epitest_prophandler:handle({plan, Plan}, Test0),
                                   #epistate{ id = ID, test_plan = Plan, test = Test }
                           end, Tests),
    %% Filter out existing epistates (looks like ets:insert_new(Tab, Objects) is not much of a help here, but FIXME?)
    Epistates = lists:filter(fun (#epistate{ id = ID }) ->
                                     case ets:lookup(EpistatesTab, ID) of
                                         [] ->
                                             true;
                                         _ -> 
                                             false
                                     end
                             end, Epistates0),
    ets:insert(EpistatesTab, Epistates),
    [ epitest_prophandler:handle({prepare, Plan}, Test) || #epistate{ test = Test } <- Epistates ].
    

initialize_workers(#state{ event_mgr = EventMgr, epistates = Epistates }) ->
    EpistateList = ets:tab2list(Epistates),
    lists:foreach(fun (Epistate) ->
                          {ok, Pid} = supervisor:start_child(epitest_test_worker_sup, [self(), Epistate]),
                          link(Pid),
                          gen_event:call(EventMgr, epitest_worker_notifier, {subscribe, Pid}),
                          ets:insert(Epistates, Epistate#epistate{ worker = Pid })
                  end, EpistateList).

start_workers(#state{ epistates = Epistates }) ->
    EpistateList = ets:tab2list(Epistates),
    lists:foreach(fun (#epistate{ worker = Pid} = Epistate0) ->
                          Epistate = Epistate0#epistate{ state = started },
                          ets:insert(Epistates, Epistate),
                          gen_fsm:send_event(Pid, start)
                  end, EpistateList).

do_lookup(ID, Epistates) ->
        case ets:lookup(Epistates, ID) of
            [] -> {error, notfound};
            [Epistate] ->
                Epistate
        end.

process_remaining_tests(#state { epistates = Epistates, event_mgr = EventMgr } = State) ->
    TestsToGo = length(ets:match(Epistates, #epistate{ _ = '_', id= '$1', state = started })),
    case TestsToGo of
        0 ->
            gen_event:notify(EventMgr, {finished, self()}),
            {next_state, finished, State};
        _ ->
            {next_state, running, State}
    end.
