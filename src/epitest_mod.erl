-module(epitest_mod).
-behaviour(gen_server).

-export([handle/2]).

-include_lib("epitest/include/epitest.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
          module,
          state
         }).

start_link(Module, Properties) ->
    gen_server:start_link({local, Module}, ?MODULE, [Module, Properties], []).

init([Module, Properties]) ->
    case apply(Module, init, [Properties]) of
        {ok, ModState} ->
            {ok, #state{
               module = Module,
               state = ModState
              }};
        Other ->
            Other
    end.

handle_call(Call, From, #state{ module = Module, state = ModState} = State) ->
    case apply(Module, handle_call, [Call, From, ModState]) of
        {reply, Reply, NewModState} ->
            {reply, Reply, State#state{ state = NewModState }};
        {reply, Reply, NewModState, hibernate} ->
            {reply, Reply, State#state{ state = NewModState }, hibernate};
        {reply, Reply, NewModState, Timeout} ->
            {reply, Reply, State#state{ state = NewModState }, Timeout};
        {noreply, NewModState} ->
            {noreply, State#state{ state = NewModState }};
        {noreply, NewModState, hibernate} ->
            {noreply, State#state{ state = NewModState }, hibernate};
        {noreply, NewModState, Timeout} ->
            {noreply, State#state{ state = NewModState }, Timeout};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#state{ state = NewModState }};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#state{ state = NewModState }}
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
-spec handle(any(), #test{}) -> #test{}.

handle(Command, InitialTest) ->
    Mods = lists:map(fun (Mod) when is_atom(Mod) -> 
                             Mod;
                         ({Mod, _}) when is_atom(Mod) ->
                             Mod
                     end,
                     proplists:get_value(mods, application:get_all_env(epitest), [])),
    case lists:foldl(fun (_Mod, {stop, Result}) ->
                             {stop, Result};
                         (Mod, {ok, Result0}) ->
                             gen_server:call(Mod, {Command, Result0})
                     end, {ok, InitialTest}, Mods) of
        {stop, Result} ->
            Result;
        {ok, Result} ->
            Result
    end.
            
