-module(epitest_ph_pending).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor = 
        case functors(Test) of
            [] ->
                [{functor, fun epitest_helpers:make_pending/0}|Descriptor0];
            _ ->
                Descriptor0
        end,
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.

functors(#test{ descriptor = Descriptor }) ->
    functors(Descriptor);
functors([{functor, F}|Rest]) when is_function(F) ->
    [F|functors(Rest)];
functors([_Property|Rest]) ->
    functors(Rest);
functors([]) ->
    [].
