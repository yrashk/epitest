-module(epitest_worker_notifier).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { workers = [] }).

init([]) ->
    {ok, #state{}}.

handle_event(#epistate{} = Epistate, #state{ workers = Workers } = State) ->
    [ gen_fsm:send_all_state_event(Worker, {notification, Epistate}) || Worker <- Workers ],
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call({subscribe, Worker}, #state{ workers = Workers } = State) ->
    {ok, ok, State#state{ workers = [Worker|Workers] }}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
