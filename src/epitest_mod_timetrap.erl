-module(epitest_mod_timetrap).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

-record(state, {
          default_timeout
         }).

init(Properties) ->
    {ok, #state{
       default_timeout = proplists:get_value(default_timeout, Properties, {30, seconds})
       }}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, #state{ default_timeout = Timeout0 } = State) ->
    Timeout = proplists:get_value(timetrap, Descriptor0, Timeout0),
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
