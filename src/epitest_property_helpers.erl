-module(epitest_property_helpers).

-include_lib("epitest/include/epitest.hrl").

-export([functors/1, remove_functors/1]).

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
remove_functors([_Property|Rest]) ->
    remove_functors(Rest);
remove_functors([]) ->
    [].
