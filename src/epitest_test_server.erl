-module(epitest_test_server).

-behaviour(gen_server).

-define(EPITEST_NOTRANSFORM, true).
-include_lib("epitest/include/epitest.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, { tests, graph }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{ tests = dict:new(), graph = digraph:new([acyclic]) }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({requires, EdgeLabel, Test}, _From, State) ->
    Reqs = do_requires(EdgeLabel, Test, State),
    {reply, Reqs, State};

handle_call({dependants, EdgeLabel, Test}, _From, State) ->
    Deps = do_dependants(EdgeLabel, Test, State),
    {reply, Deps, State};

handle_call({all_dependants, EdgeLabel, Test}, _From, State) ->
    Desc = do_all_dependants(EdgeLabel, Test, State),
    {reply, Desc, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_module, Mod}, State) ->
    lists:foreach(fun ({_Line,T}) ->
			  digraph:add_vertex(State#state.graph, {Mod, T,[]})
		  end, Mod:tests()),
    [ add_edges(State#state.graph, Mod, Edge) || Edge <- [r,ir,fr] ],
    {noreply, State};

handle_cast(run, State) ->
    Tests = lists:foldl(fun ({Mod,T,A},Tests) ->
				{ok, Worker} = supervisor:start_child(epitest_test_sup, [#epistate{test={Mod,T,A}}]),
				dict:store({Mod,T,A}, Worker, Tests)
			end, State#state.tests, digraph:vertices(State#state.graph)),
    {noreply, State#state{tests = Tests}};

handle_cast({notify, Tests, Kind, Epistate, Test}, State) ->
    NewTests = dict:erase(Test, State#state.tests),
    spawn(fun () -> wipeout_unreachables(Kind, NewTests, Test) end),
    lists:foreach(fun (T) ->
			  case dict:find(T, State#state.tests) of
			      {ok, TestPid} ->
				  gen_fsm:send_event(TestPid, {notification, Kind, Epistate, Test});
			      _ ->
				  skip
			  end
		  end, Tests),
    do_check_tests_presence(NewTests),
    {noreply, State#state{tests = NewTests}};

handle_cast({wipeout, Tests}, State) ->
    NewTests = lists:foldl(fun (X,D) -> dict:erase(X,D) end, State#state.tests, Tests),
    do_check_tests_presence(State#state.tests),
    {noreply, State#state{ tests = NewTests} };

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
add_dep(D,Dep, Mod, Name, Edge) when is_list(Dep) ->
     digraph:add_edge(D, {Mod, Name,[]}, {Mod, Dep,[]}, Edge);
add_dep(D,{Mod1, Dep}, Mod, Name, Edge) when is_list(Dep) ->
    digraph:add_vertex(D, {Mod1, Dep, []}),
     digraph:add_edge(D, {Mod, Name,[]}, {Mod1, Dep,[]}, Edge);
add_dep(D,{'CORE',"All dependants",[Mod0,Name0,Edge0]}=F, Mod, Name, Edge) ->
    digraph:add_vertex(D, F),
    digraph:add_edge(D, F, {Mod0, Name0,[]}, Edge0),
    digraph:add_edge(D, {Mod, Name,[]}, F, Edge);
add_dep(D,{Mod1, Dep, Args}, Mod, Name, Edge) when is_list(Dep) ->
    digraph:add_vertex(D, {Mod1, Dep, Args}),
    digraph:add_edge(D, {Mod, Name,[]}, {Mod1, Dep, Args}, Edge).

add_edge({_, Name}, D, Mod, Edge) when is_list(Name) ->
    Info = apply(Mod, test, [Name]),
    Deps = proplists:get_value(Edge, Info, []),
    lists:foreach(fun(Dep) -> add_dep(D,Dep, Mod, Name, Edge) end, Deps);
add_edge({_, Name}=P, D, Mod, Edge) ->
    ok. 

add_edges(D, Mod, Edge) ->
    lists:foreach(fun (T) -> add_edge(T, D, Mod, Edge) end, Mod:tests()).

do_requires(EdgeLabel, Test, State) ->
    ReqEdges = lists:filter(fun (E) ->
				    case digraph:edge(State#state.graph, E) of
					{_,_,_, EdgeLabel} ->
					    true;
					_ ->
					    false
				    end
			    end, digraph:out_edges(State#state.graph, Test)),
    lists:map(fun (E) -> {_,_,D,_} = digraph:edge(State#state.graph, E), D end, ReqEdges).

do_dependants(EdgeLabel, Test, State) ->
    DepEdges = lists:filter(fun (E) ->
				filter_dependants(E, EdgeLabel, State)
			end, digraph:in_edges(State#state.graph, Test)),
    lists:map(fun (E) -> {_,D,_,_} = digraph:edge(State#state.graph, E), D end, DepEdges).
    
do_all_dependants(EdgeLabel, Test, State) ->
    Deps = do_dependants(EdgeLabel, Test, State), 
    lists:flatten(lists:map(fun (X) ->
				    [Deps|do_all_dependants(EdgeLabel, X, State)]
			    end,
			    Deps)).

filter_dependants(_E, '_',_State) ->
    true;
filter_dependants(E, EdgeLabel,State) ->
    case digraph:edge(State#state.graph, E) of
	{_,_,_, EdgeLabel} ->
	    true;
	_ ->
	    false
    end.
    
wipeout_unreachables(failed, Tests, Test) ->
    gen_server:cast(epitest_test_server, {wipeout, epitest:dependants(Test, r)});
wipeout_unreachables(passed, Tests, Test) ->
    gen_server:cast(epitest_test_server, {wipeout, epitest:dependants(Test, fr)}).

do_check_tests_presence(Tests) ->
    DictSize = dict:size(Tests),
    if DictSize == 0 ->
	    gen_event:notify(epitest_log, finished);
       true ->
	    skip
    end.
