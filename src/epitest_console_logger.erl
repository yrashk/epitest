-module(epitest_console_logger).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { passed = 0, failed = 0, pending = 0, unreachable = 0,
                 stacktraces = []
               }).

init([]) ->
    {ok, #state{}}.

handle_event(#epistate{ state = succeeded, test = Test }, State) ->
    #test{ loc = Loc, descriptor = Descriptor } = Test,
    case lists:member(hidden, Descriptor) of
        false ->
            io:format("\e[32m[PASSED] \e[32m \e[37m~s\e[32m (~s)\e[0m~n", [format_name(Test), format_loc(Loc)]),
            {ok, State#state{ passed = State#state.passed + 1} };
        true ->
            {ok, State}
    end;

handle_event(#epistate{ state = {failed, {{pending, Description}, _Stacktrace}}, test = Test }, State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[33m[PENDNG]  ~s (~s): ~p\e[0m~n", [format_name(Test), format_loc(Loc), Description]),
    {ok, State#state{ pending = State#state.pending + 1} };

handle_event(#epistate{ state = {failed, {failed_requirement, Type, FRTest}}, test = Test }, State) ->
    #test{ loc = Loc } = Test,
    #test{ loc = FRLoc } = FRTest,
    io:format("\e[36m[UNREAC]  ~s (~s): expected \"~s\" (~s) to be a ~p\e[0m~n", [format_name(Test), format_loc(Loc), format_name(FRTest), format_loc(FRLoc), Type]),
    {ok, State#state{ unreachable = State#state.unreachable + 1 } };

handle_event(#epistate{ state = {failed, {Reason, Stacktrace}}, test = Test }, #state{ stacktraces = Stacktraces } = State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[31m[FAILED] \e[32m \e[37m~s\e[32m (~s):\e[31m ~s (stacktrace #~w)\e[0m~n", [format_name(Test), format_loc(Loc), format_reason(Reason), length(Stacktraces) + 1]),
    {ok, State#state{ failed = State#state.failed + 1,
                      stacktraces = Stacktraces ++ [Stacktrace]
                    } };

handle_event({finished, _Plan}, State) ->
    io:format("~n\e[4m\e[32mPassed: ~w \e[31mFailed: ~w \e[33mPending: ~w \e[36mUnreachable: ~w\e[24m\e[0m~n",[State#state.passed, State#state.failed, State#state.pending, State#state.unreachable]),
    %% Dump stacktraces
    io:format("~n\e[4m\e[31mStracktraces:\e[24m~n~n"),
    lists:foldl(fun (Stacktrace, Index) ->
                        io:format("\e[37m#~w)~n\e[31m~s~n~n", [Index, format_stacktrace(Stacktrace)]),
                        Index + 1
                end, 1, State#state.stacktraces),
    io:format("\e[0m~n"),
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

format_name(#test{ loc = {module, {_, Prefix}, _}, signature = Title}) when is_list(Title), is_list(Prefix), length(Prefix) > 0 ->
    io_lib:format("\e[4m~s\e[24m: ~s",[Prefix, Title]);    
format_name(#test{ loc = {module, {_, Prefix}, _}, signature = {Title, Args}}) when is_list(Title), is_list(Prefix), length(Prefix) > 0, is_list(Args) ->
    io_lib:format("\e[4m~s\e[24m: ~s(~p)",[Prefix, Title, Args]);    
format_name(#test{ signature = Title}) when is_list(Title) ->
    io_lib:format("~s",[Title]);
format_name(#test{ signature = {Title, Args}}) when is_list(Title), is_list(Args) ->
    io_lib:format("~s(~p)",[Title, Args]).

format_loc({module, {Module, _}, Line}) ->
    io_lib:format("~w.erl:~p",[Module, Line]);
format_loc(dynamic) ->
    io_lib:format("dynamic").

format_reason(negative) ->
    io_lib:format("This test should have failed", []);
format_reason(Reason) ->
    io_lib:format("~p",[Reason]).

format_stacktrace([{Module, Fun, Line}|Rest]) ->
    [io_lib:format("     ~p.erl:~p ~p~n", [Module, Line, Fun])|format_stacktrace(Rest)];
format_stacktrace([]) ->
    [].
