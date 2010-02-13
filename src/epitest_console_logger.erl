-module(epitest_console_logger).

-behaviour(gen_event).

-define(EPITEST_NOTRANSFORM, true).
-include_lib("epitest/include/epitest.hrl").

%% API
-export([start/0, start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {passed=0, failed=0, pending=0, elapsed=0}).
-define(SERVER, ?MODULE).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}). 

%%--------------------------------------------------------------------
%% Function: add_handler() -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------

handle_event({_, #epistate{test = {'CORE',_,_}}}, State) ->
    {ok, State}; % skip core tests
handle_event({success, #epistate{ test = {M, T, A} } = Epistate}, State) ->
    case proplists:get_value(hidden, get_info(Epistate)) of
        true ->
            {ok, State#state{elapsed = State#state.elapsed + Epistate#epistate.elapsed} };
        undefined ->
            io:format("\e[32m[PASSED] \e[90m~-20w\e[32m (~fs) ~p:'~s'(~200p)\e[0m~n", [enode(Epistate),Epistate#epistate.elapsed/1000000,M,T,A]),
            {ok, State#state{elapsed = State#state.elapsed + Epistate#epistate.elapsed, passed=State#state.passed + 1} }
    end;
handle_event({failure, #epistate{failure={epitest_pending, Reason}}=Epistate}, State) ->
    {M,T,A} = Epistate#epistate.test,
    io:format("\e[33m[PENDNG] \e[90m~-20w\e[33m (~fs) ~p:'~s'(~200p): ~200p\e[0m~n", [enode(Epistate),Epistate#epistate.elapsed/1000000,M,T,A,Reason]),
    {ok, State#state{elapsed = State#state.elapsed + Epistate#epistate.elapsed, pending=State#state.pending + 1} };
handle_event({failure, Epistate}, State) ->
    {M,T,A} = Epistate#epistate.test,
    io:format("\e[31m[FAILED] \e[90m~-20w\e[31m (~fs) ~p:'~s'(~200p): ~200p\e[0m~n", [enode(Epistate),Epistate#epistate.elapsed/1000000,M,T,A,Epistate#epistate.failure]),
    {ok, State#state{elapsed = State#state.elapsed + Epistate#epistate.elapsed, failed=State#state.failed + 1} };
handle_event(finished, State) ->
    io:format("\e[32mPassed: ~w \e[31mFailed: ~w \e[33mPending: ~w\e[0m~nTotal time elapsed: ~fs~n",[State#state.passed, State#state.failed, State#state.pending, State#state.elapsed/1000000]),
    epitest_slave:stop_server(),
    erlang:halt(),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
enode(#epistate{}=Epistate) ->
    proplists:get_value(splitnode, Epistate#epistate.options, node()).
    
get_info(Epistate) ->
    Test = Epistate#epistate.test,
    {Mod, Name, Args} = Test,
    case Test of
	{'CORE', "All dependants", [M,T,E]} ->
	    [{r, [{M,T,E}]},{f, fun () -> ok end}];
	{_,_,[]} ->
	    apply(Mod, test, [Name]);
	_ ->
		apply(Mod, test, [list_to_tuple([Name|Args])])
    end.

%%--------------------------------------------------------------------
%%% Public functions
%%--------------------------------------------------------------------
start() ->
    gen_event:add_handler(epitest_log, ?SERVER, []),
    start_link().
