-module(epitest_console_runner).
-behaviour(gen_event).

-include_lib("epitest/include/epitest.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([finish/0]).

init([]) ->
    {ok, undefined}.

handle_event({finished, Plan}, State) ->
    Pid = proplists:get_value(console_runner_master_proc, application:get_all_env(epitest), self()),
    case os:getenv("EPITEST_ATTACH") of
        false ->
            Pid ! {epitest_console_plan_finished, Plan};
        _ ->
            io:format("~n~n  ====> You can attach to this node ( erl -sname console -remsh ~s )~n~n", [atom_to_list(node())]),
            ignore
    end,
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

finish() ->
    Pid = proplists:get_value(console_runner_master_proc, application:get_all_env(epitest)),
    Pid ! epitest_console_plans_finished.
