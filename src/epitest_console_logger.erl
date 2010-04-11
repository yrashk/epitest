-module(epitest_console_logger).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { passed = 0, failed = 0, pending = 0, unreachable = 0,
                 failures = [],
                 elapsed = 0,
                 started
               }).

init([]) ->
    {ok, #state{}}.

handle_event(#epistate{ state = succeeded, test = Test, elapsed = Elapsed, node = Node }, State) ->
    #test{ loc = Loc, descriptor = Descriptor } = Test,
    case lists:member(hidden, Descriptor) of
        false ->
            io:format("\e[32m[PASSED] \e[32m \e[37m~s\e[32m (~s) [~w/~s]\e[0m~n", [format_name(Test), format_loc(Loc), Node, format_elapsed(Elapsed)]),
            {ok, State#state{ passed = State#state.passed + 1, elapsed = State#state.elapsed + Elapsed} };
        true ->
            {ok, State#state{ elapsed = State#state.elapsed + Elapsed} }
    end;

handle_event(#epistate{ state = {failed, {{pending, Description}, _Stacktrace}}, elapsed = Elapsed, test = Test, node = Node }, State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[33m[PENDNG]  ~s (~s): ~p [~w/~s] \e[0m~n", [format_name(Test), format_loc(Loc), Description, Node, format_elapsed(Elapsed)]),
    {ok, State#state{ pending = State#state.pending + 1,
                      elapsed = State#state.elapsed + Elapsed
                    } };

handle_event(#epistate{ state = {failed, {failed_requirement, Type, FRTest}}, test = Test, elapsed = Elapsed, node = Node }, State) ->
    #test{ loc = Loc } = Test,
    #test{ loc = FRLoc } = FRTest,
    io:format("\e[36m[UNREAC]  ~s (~s): expected \"~s\" (~s) to be a ~p [~w/~s]\e[0m~n", [format_name(Test), format_loc(Loc), format_name(FRTest), format_loc(FRLoc), Type, Node, format_elapsed(Elapsed)]),
    {ok, State#state{ unreachable = State#state.unreachable + 1, elapsed = State#state.elapsed + Elapsed } };

handle_event(#epistate{ state = {failed, {_Reason, _Stacktrace}=Failure}, test = Test, elapsed = Elapsed, node = Node}, #state{ failures = Failures } = State) ->
    #test{ loc = Loc } = Test,
    io:format("\e[31m[FAILED] \e[32m \e[37m~s\e[32m (~s) \e[31m[~w/~s] \e[4m(failure #~w)\e[24m\e[0m~n", [format_name(Test), format_loc(Loc), Node, format_elapsed(Elapsed), length(Failures) + 1]),
    {ok, State#state{ failed = State#state.failed + 1,
                      failures = [Failure|Failures],
                      elapsed = State#state.elapsed + Elapsed
                    } };

handle_event({started, _Plan}, State) ->
    {ok, State#state{ started = now() }};

handle_event({finished, _Plan}, State) ->
    Finished = now(),
    Diff = timer:now_diff(Finished, State#state.started),
    %% Stats
    io:format("~n\e[4m\e[32mPassed: ~w \e[31mFailed: ~w \e[33mPending: ~w \e[36mUnreachable: ~w\e[24m\e[0m~n",[State#state.passed, State#state.failed, State#state.pending, State#state.unreachable]),
    %% Timing
    io:format("Time elapsed: ~s, actual run time: ~s~n",[format_elapsed(State#state.elapsed), format_elapsed(Diff)]),
    %% Dump stacktraces
    io:format("~n\e[4m\e[31mFailures:\e[24m~n~n"),
    lists:foldl(fun ({Reason, Stacktrace}, Index) ->
                        io:format("\e[37m#~w) ~s ~n\e[31m~s~n", [Index, format_reason(Reason), format_stacktrace(Stacktrace)]),
                        Index + 1
                end, 1, lists:reverse(State#state.failures)),
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
format_reason({timetrapped, {X, Unit}}) ->
    io_lib:format("timetrapped at ~w ~w",[X, Unit]);
format_reason(Reason) ->
    io_lib:format("~p",[Reason]).

format_stacktrace([{Module, Fun, Line}|Rest]) ->
    [io_lib:format("     ~p.erl:~p ~p~n", [Module, Line, Fun])|format_stacktrace(Rest)];
format_stacktrace([]) ->
    [].

format_elapsed(Elapsed) ->
    io_lib:format("~fs", [Elapsed/1000000]).
