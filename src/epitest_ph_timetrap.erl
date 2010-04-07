-module(epitest_ph_timetrap).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Timeout = proplists:get_value(timetrap, Descriptor0, 
                                  proplists:get_value(timetrap_threshold, 
                                                      application:get_all_env(epitest), 
                                                      {30, seconds})),

    Descriptor = [{functor, start_timetrap(Timeout)}|Descriptor0],
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.

%%
%% Internal functions
%%

start_timetrap(Timeout) ->
    Ms = milliseconds(Timeout),
    fun (#epistate{ worker = Worker }) ->
            {ok, _Tref} = timer:apply_after(Ms, erlang, apply, [fun () ->
                                                                        gen_fsm:send_event(Worker, {failure, {{timetrapped, Timeout}, []}}),
                                                                        exit(timetrapped)
                                                                end, []])
    end.



milliseconds({N, seconds}) ->
    N * 1000;
milliseconds({N, milliseconds}) ->
    N.
