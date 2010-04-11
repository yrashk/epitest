-module(epitest_property_helpers).

-include_lib("epitest/include/epitest.hrl").

-export([functors/1, remove_functors/1, run_functors/2]).

%%
%% Exports
%%

functors(#test{ descriptor = Descriptor }) ->
    functors(Descriptor);
functors([{functor, F}|Rest]) when is_function(F) ->
    [F|functors(Rest)];
functors([_Property|Rest]) ->
    functors(Rest);
functors([]) ->
    [].


remove_functors(#test{ descriptor = Descriptor }) ->
    remove_functors(Descriptor);
remove_functors([{functor, F}|Rest]) when is_function(F) ->
    remove_functors(Rest);
remove_functors([Property|Rest]) ->
    [Property|remove_functors(Rest)];
remove_functors([]) ->
    [].

run_functors([F|Fs], Epistate) when is_function(F, 0) ->
    [F()|run_functors(Fs, Epistate)];
run_functors([F|Fs], Epistate) when is_function(F, 1) ->
    [F(Epistate)|run_functors(Fs, Epistate)];
run_functors([F|_Fs], #epistate{ test = Test }) when is_function(F) ->
    throw({badarity, Test, F});
run_functors([], _Epistate) ->
    [].
