-module(epitest_slave_server).
-behaviour(gen_server).

-include_lib("epitest/include/epitest.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([create/0, launch/1, eval/2]).

-record(state, {
          slaves,
          launch_counter
         }).


-define(SERVER, {?MODULE, epitest_cluster:name()}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{
       slaves = ets:new(slaves, [public]),
       launch_counter = 0
      }}.

handle_call(create, _From, #state{ slaves = Slaves } = State) ->
    Ref = make_ref(),
    ets:insert(Slaves, {Ref, undefined}),
    {reply, Ref, State};

handle_call({launch, Ref}, _From, #state{ slaves = Slaves, launch_counter = Counter } = State) ->
    case ets:lookup(Slaves, Ref) of
        [] ->
            {reply, {error, nosuchnode}, State};
        [{Ref, _}] ->
            {ok, Hostname} = inet:gethostname(),
            Name = list_to_atom("slave" ++ erlang:integer_to_list(Counter + 1)),
            Paths = get_paths(),
            {ok, Node} = slave:start(list_to_atom(Hostname), Name, "-hidden"),
            ok = rpc:call(Node, code, add_paths, [Paths]),
            ets:insert(Slaves, {Ref, Node}),
            {reply, {ok, Node}, State#state{ launch_counter = Counter + 1}}
    end;

handle_call({eval, Ref, Fun}, _From, #state{ slaves = Slaves } = State) ->
    case ets:lookup(Slaves, Ref) of
        [] ->
            {reply, {error, nosuchnode}, State};
        [{Ref, Node}] ->
            Result0 = rpc:call(Node, erlang, apply, [Fun, []]),
            Result =
            case Result0 of
                {badrpc, R} ->
                    R;
                R ->
                    R
            end,
            {reply, {ok, Result}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Public functions
-spec create() -> any().

create() ->
    gen_server:call({global, ?SERVER}, create).

-spec launch(any()) -> {'ok', node()} | {'error', any()}.
                     
launch(Ref) ->
    gen_server:call({global, ?SERVER}, {launch, Ref}).

-spec eval(any(), fun()) -> any().

eval(Ref, Fun) ->
    gen_server:call({global, ?SERVER}, {eval, Ref, Fun}).


%% Internal function
get_paths() ->
    lists:map(fun filename:absname/1, code:get_path()).  
