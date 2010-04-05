-module(epitest_console_logger).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { passed = 0, failed = 0 }).

init([]) ->
    {ok, #state{}}.

handle_event(#epistate{ state = succeeded, test = Test }, State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[32m[PASSED] \e[32m \e[37m~s\e[32m(~s)\e[0m~n", [format_name(Test), format_loc(Loc)]),
    {ok, State#state{ passed = State#state.passed + 1} };

handle_event(#epistate{ state = {failed, {failed_requirement, Type, FRTest}}, test = Test }, State) ->
    #test{ loc = Loc } = Test,
    #test{ loc = FRLoc } = FRTest,
    io:format("\e[36m[UNREAC]  ~s(~s): expected \"~s\"(~s) to be a ~p\e[0m~n", [format_name(Test), format_loc(Loc), format_name(FRTest), format_loc(FRLoc), Type]),
    {ok, State#state{ failed = State#state.failed + 1} };

handle_event(#epistate{ state = {failed, Res}, test = Test }, State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[31m[FAILED] \e[32m \e[37m~s\e[32m(~s):\e[31m~200p\e[0m~n", [format_name(Test), format_loc(Loc), Res]),
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

%% Internal functions

format_name(#test{ loc = {module, {_, Prefix}, _}, signature = Title}) when is_list(Title), length(Prefix) > 0 ->
    io_lib:format("\e[4m~s\e[24m: ~s",[Prefix, Title]);    
format_name(#test{ signature = Title}) when is_list(Title) ->
    io_lib:format("~s",[Title]).

format_loc({module, {Module, _}, Line}) ->
    io_lib:format("~w.erl:~p",[Module, Line]);
format_loc(dynamic) ->
    io_lib:format("dynamic").
