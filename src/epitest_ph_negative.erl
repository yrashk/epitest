-module(epitest_ph_negative).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor =
        case lists:member(negative, Descriptor0) of
            true ->
                rewrite_functors(Test);
            _ ->
                Descriptor0
        end,
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.

rewrite_functors(#test{ descriptor = Descriptor }) ->
    rewrite_functors(Descriptor);
rewrite_functors([{functor, F}|Rest]) when is_function(F) ->
    [{functor, negative_functor(F)}|rewrite_functors(Rest)];
rewrite_functors([_Property|Rest]) ->
    rewrite_functors(Rest);
rewrite_functors([]) ->
    [].

negative_functor(F) when is_function(F, 0) ->
    fun () ->
            try 
                F(),
                epitest_helpers:fail(negative)
            catch
                error:negative ->
                    erlang:error(negative);
                error:_Reason -> 
                    suppress
            end
    end;
negative_functor(F) when is_function(F, 1) ->
    fun (Epistate) ->
            try
                F(Epistate),
                epitest_helpers:fail(negative)
            catch
                error:negative ->
                    erlang:error(negative);
                error:_Reason ->
                    suppress
            end
    end.
