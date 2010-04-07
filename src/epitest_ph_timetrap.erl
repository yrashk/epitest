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

    Fun = timetrapize(Timeout, epitest_property_helpers:functors(Test)),
    Descriptor = [{functor, Fun}|epitest_property_helpers:remove_functors(Test)],
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.

%%
%% Internal functions
%%

timetrapize(Timeout, Funs) ->
    Ms = milliseconds(Timeout),
    fun (Epistate) ->
            timer:exit_after(Ms, {timetrapped, Timeout}),
            F = combined_functor(Funs),
            F(Epistate)
    end.


combined_functor(Funs) ->
    fun (Epistate) ->
            lists:map(fun (F) when is_function(F, 0) ->
                              F();
                          (F) when is_function(F, 1) ->
                              F(Epistate)
                      end, Funs)
    end.

milliseconds({N, seconds}) ->
    N * 1000;
milliseconds({N, milliseconds}) ->
    N.
