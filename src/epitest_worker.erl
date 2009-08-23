-module(epitest_worker).

-behaviour(gen_fsm).

-define(EPITEST_NOTRANSFORM, true).
-include_lib("epitest/include/epitest.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, 

	 initialized/2, ready/2, running/2, waiting/2,

	 handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([status/1]).

-record(state, {epistate, 
		waiting_r=[],
		waiting_ir=[],
		waiting_fr=[]
	       }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Epistate) ->
    gen_fsm:start_link(?MODULE, [Epistate], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([Epistate]) ->
    gen_fsm:send_event(self(), start),
    {ok, initialized, #state{epistate=Epistate}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
initialized(start, State) ->
    Epistate = State#state.epistate,
    case lists:flatten([epitest:requires(Epistate#epistate.test, Label) || Label <- [r,ir,fr]]) of
	[] ->
	    gen_fsm:send_event(self(), run),
	    {next_state, ready, State};
	_ ->
	    {next_state, waiting, State#state{epistate=Epistate,
				 waiting_r=epitest:requires(Epistate#epistate.test, r),
				 waiting_ir=epitest:requires(Epistate#epistate.test, ir),
				 waiting_fr=epitest:requires(Epistate#epistate.test, fr)
				}}
    end.
    

ready(run, State) ->
    Pid = self(),
    Epistate0 = (State#state.epistate),
    Info = get_info(State),
    Nodesplit = proplists:get_value(nodesplit, Info, false),
    State1 = case Nodesplit of
		 true ->
		     {ok, Node} = epitest_slave:start_link(),
		     State#state{ epistate = Epistate0#epistate{ options = lists:keystore(splitnode, 1, Epistate0#epistate.options, proplists:property(splitnode, Node))}};
		 L when is_list(L) ->
		     {ok, Node} = epitest_slave:start_link(proplists:get_value(args, L, [])),
		     State#state{ epistate = Epistate0#epistate{ options = lists:keystore(splitnode, 1, Epistate0#epistate.options, proplists:property(splitnode, Node))}};
		 false ->
		     State
	     end,
    spawn(fun () -> do_run(Pid,Info, State1) end),
    {next_state, running, State1}.

running({success, {epitest_variables, Vars}}, State) ->
    Epistate = merge_vars(State, Vars),
    gen_event:notify(epitest_log, {success, Epistate}),
    NotificationList = lists:flatten([epitest:dependants((State#state.epistate)#epistate.test, Label) || Label <- [r,ir]]),
    gen_server:cast(epitest_test_server, {notify, NotificationList, passed, Epistate, (State#state.epistate)#epistate.test}),
    {next_state, passed, State#state{ epistate = Epistate}};

running({success, _}, State) ->
    gen_fsm:send_event(self(), {success, {epitest_variables, []}}),
    {next_state, running, State};

running({pending, Reason}, State) ->
    Epistate0 = (State#state.epistate),
    Epistate = Epistate0#epistate{ failure = {epitest_pending, Reason} },
    gen_event:notify(epitest_log, {failure, Epistate}),
    NotificationList = lists:flatten([epitest:dependants((State#state.epistate)#epistate.test, Label) || Label <- [ir,fr]]),
    gen_server:cast(epitest_test_server, {notify, NotificationList, failed, Epistate, (State#state.epistate)#epistate.test}),
    {next_state, failed, State#state{epistate=Epistate}};

running({failure, Result}, State) ->
    Epistate0 = (State#state.epistate),
    Info = get_info(State),
    Epistate = Epistate0#epistate{ failure = {proplists:get_value(fmsg, Info, "Exception caught"), Result} },
    gen_event:notify(epitest_log, {failure, Epistate}),
    NotificationList = lists:flatten([epitest:dependants((State#state.epistate)#epistate.test, Label) || Label <- [ir,fr]]),
    gen_server:cast(epitest_test_server, {notify, NotificationList, failed, Epistate, (State#state.epistate)#epistate.test}),
    {next_state, failed, State#state{epistate=Epistate}}.

waiting({notification, passed, Epistate0, Test}, #state{waiting_ir=[Test], waiting_r=[Test], waiting_fr=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};
waiting({notification, passed, Epistate0, Test}, #state{waiting_ir=[Test], waiting_r=[], waiting_fr=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};
waiting({notification, passed, Epistate0, Test}, #state{waiting_ir=[],waiting_r=[Test], waiting_fr=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};

waiting({notification, passed, Epistate0, Test}, #state{}=State) ->
    Epistate = merge(State, Epistate0),
    {next_state, waiting, State#state{epistate=Epistate, waiting_ir=State#state.waiting_ir -- [Test], waiting_r=State#state.waiting_r -- [Test]}};

waiting({notification, failed, Epistate0, Test}, #state{waiting_ir=[Test], waiting_fr=[Test], waiting_r=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};
waiting({notification, failed, Epistate0, Test}, #state{waiting_ir=[Test], waiting_fr=[], waiting_r=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};
waiting({notification, failed, Epistate0, Test}, #state{waiting_ir=[],waiting_fr=[Test], waiting_r=[]}=State) ->
    Epistate = merge(State, Epistate0),
    gen_fsm:send_event(self(), run),
    {next_state, ready, #state{epistate=Epistate}};

waiting({notification, failed, Epistate0, Test}, #state{}=State) ->
    Epistate = merge(State, Epistate0),
    {next_state, waiting, State#state{epistate = Epistate, waiting_ir=State#state.waiting_ir -- [Test], waiting_fr=State#state.waiting_fr -- [Test]}}.


%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
%state_name(_Event, _From, State) ->
%    Reply = ok,
%    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(status, _From, StateName, State) ->
    {reply, {StateName, State}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_run(Pid,Info,State) ->
    Epistate = (State#state.epistate),
    Opts = Epistate#epistate.options,
    F = proplists:get_value(f, Info, fun () -> skip end),
    Args = if 
	       is_function(F,0) ->
		   [];
	       is_function(F, 1) ->
		   [Epistate];
	       true ->
		   throw("'f' fun can be either /0 or /1")
	   end,
    N = proplists:get_value(negative, Info, false),
    Nodesplit = proplists:get_value(splitnode, Opts) =/= undefined,
    try
	Result = case Nodesplit of
		     true ->
			 rpc:call(proplists:get_value(splitnode, Opts), erlang, apply, [F,Args]);
		     _ ->
			 apply(F,Args)
		 end,
	case Result of
	    {badrpc, {'EXIT', Err}} ->
		report_result(Pid, Info, State, Err, N);
	    _ ->
		report_result(Pid, Info, State, Result, true and not N)
	end
    catch K:V ->
	    report_result(Pid, Info, State, {K,V}, N)
    end.

report_result(Pid, Info, State, Result, true) ->
    Repeat = proplists:get_value(repeat, Info),
    case Repeat of
	{until, Result} ->
	    gen_fsm:send_event(Pid, {success, Result});
	{until, _} ->
	    do_run(Pid, Info, State);
	{while, Result} ->
	    do_run(Pid, Info, State);
	{while, _} ->
	    gen_fsm:send_event(Pid, {success, Result});
	_ ->
	    gen_fsm:send_event(Pid, {success, Result})
    end;
report_result(Pid, _, _, {throw, {epitest_pending, Reason}}, false) ->
    gen_fsm:send_event(Pid, {pending, Reason});
report_result(Pid, _, _, Result, false) ->
    gen_fsm:send_event(Pid, {failure, Result}).


get_info(State) ->
    Epistate = State#state.epistate,
    Test = Epistate#epistate.test,
    {Mod, Name, Args} = Test,
    Info0 =
	case Test of
	    {'CORE', "All dependants", [M,T,E]} ->
		[{r, [{M,T,E}]}];
	    {_,_,[]} ->
		apply(Mod, test, [Name]);
	    _ ->
		apply(Mod, test, [list_to_tuple([Name|Args])])
	end,
    Info0 ++ [{f,epitest_helpers:fpending()}].

merge_vars(State, Vars) ->
    Epistate0 = (State#state.epistate),
    NewVars = Vars ++ Epistate0#epistate.variables,
    Epistate0#epistate{ variables = NewVars }.

merge(State, Epistate1) ->
    Epistate0 = (State#state.epistate),
    NewOpts = Epistate1#epistate.options ++ Epistate0#epistate.options,
    NewVars = Epistate1#epistate.variables ++ Epistate0#epistate.variables,
    Epistate0#epistate{ options = NewOpts, variables = NewVars }.

%%--------------------------------------------------------------------
%%% Public functions
%%--------------------------------------------------------------------
status(Worker) ->
    gen_fsm:sync_send_all_state_event(Worker, status).
