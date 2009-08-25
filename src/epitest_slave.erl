-module(epitest_slave).
-export([start_link/0, start_link/1, start_link/2]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
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
    {ok, Node}.

generate_nodename() ->
    S = [$s,$l,$a,$v,$e|erlang:integer_to_list(gen_server:call(?SERVER, incr))],
    {S, erlang:list_to_atom(S)}.
       
