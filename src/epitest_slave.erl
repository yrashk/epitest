-module(epitest_slave).
-export([start_link/0, start_link/1, start_link/2]).
-export([block_call/4, block_call/5]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(REXNAME, epitest_slave_rex).

-record(state, { counter = 0 }).

%% API
-export([start_server_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_server_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(incr, _From, State0) ->
    State = State0#state{ counter = State0#state.counter + 1},
    {reply, State#state.counter, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
get_path() ->
    lists:map(fun filename:absname/1, code:get_path()).    

start_link() ->
    start_link([]).

start_link(Args) ->
    {_SNodename, Nodename} = generate_nodename(),
    start_link(Nodename, Args).

start_link(Nodename, Args) ->
    {ok, Host} = inet:gethostname(),
    Paths = get_path(),
    {ok, Node} = slave:start_link(list_to_atom(Host), Nodename, Args),
    ok = rpc:call(Node, code, add_paths, [Paths]),
    {ok, _} = rpc:call(Node, gen_server, start, [{local,?REXNAME},rpc,[],[]]),
    {ok, Node}.

generate_nodename() ->
    S = [$s,$l,$a,$v,$e|erlang:integer_to_list(gen_server:call(?SERVER, incr))],
    {S, erlang:list_to_atom(S)}.


block_call(N,M,F,A) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity).

block_call(N,M,F,A,infinity) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity);
block_call(N,M,F,A,Timeout) when is_integer(Timeout), Timeout >= 0 ->
    do_call(N, {block_call,M,F,A,group_leader()}, Timeout).

do_call(Node, Request, infinity) ->
    rpc_check(catch gen_server:call({?REXNAME,Node}, Request, infinity));
do_call(Node, Request, Timeout) ->
    Tag = make_ref(),
    {Receiver,Mref} =
	erlang:spawn_monitor(
	  fun() ->
		  process_flag(trap_exit, true),
		  Result = gen_server:call({?REXNAME,Node}, Request, Timeout),
		  exit({self(),Tag,Result})
	  end),
    receive
	{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
	    rpc_check(Result);
	{'DOWN',Mref,_,_,Reason} ->
	    rpc_check_t({'EXIT',Reason})
    end.
rpc_check_t({'EXIT', {timeout,_}}) -> {badrpc, timeout};
rpc_check_t(X) -> rpc_check(X).
	    
rpc_check({'EXIT', {{nodedown,_},_}}) -> {badrpc, nodedown};
rpc_check({'EXIT', X}) -> exit(X);
rpc_check(X) -> X.
