-module(epitest_mod_pending).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor = 
        case epitest_property_helpers:functors(Test) of
            [] ->
                [{functor, fun epitest_helpers:make_pending/0}|Descriptor0];
            _ ->
                Descriptor0
        end,
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.
