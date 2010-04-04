-module(epitest_console_logger).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { passed = 0, failed = 0 }).

init([]) ->
    {ok, #state{}}.

handle_event(#epistate{ state = succeeded, test = Test } = Epistate, State) ->
    #test{ loc = Loc,
           signature = Signature } = Test,
    io:format("\e[32m[PASSED] \e[32m ~p:\e[37m'~p'\e[32m\e[0m~n", [Loc, Signature]),
    {ok, State#state{ passed = State#state.passed + 1} };

handle_event(#epistate{ state = {failed, Res}, test = Test } = Epistate, State) ->
    #test{ loc = Loc,
           signature = Signature } = Test,
    io:format("\e[31m[FAILED] \e[32m ~p:\e[37m'~p'\e[32m: ~200p\e[0m~n", [Loc, Signature, Res]),
    {ok, State#state{ failed = State#state.failed + 1} };

handle_event({finished, _Plan}, State) ->
    io:format("\e[32mPassed: ~w \e[31mFailed: ~w\e[0m~n",[State#state.passed, State#state.failed]),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
