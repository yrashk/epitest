-module(epitest_mod_skip).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor =
        case lists:member(skip, Descriptor0) of
            true ->
                [hidden|epitest_property_helpers:remove_functors(Test)];
            _ ->
                Descriptor0
        end,
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.
