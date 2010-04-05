-module(epitest_beam).
-export([signatures/1]).
-include_lib("epitest/include/epitest.hrl").

-spec signatures(module()) -> list(test_signature()).

signatures(Module) ->								
    {Module, ModuleBinary, _Filename} = code:get_object_code(Module),
    case beam_lib:chunks(ModuleBinary, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, AC}}]}} ->
            forms(AC);
        {error, beam_lib, _Reason} ->
	    % TODO: issue a warning or actually handle errors everywhere where we use this
            []
    end.

%% Internal functions
forms(L) ->
    forms(L, []).

forms([], _Prefix) ->
    [];
forms([{attribute, _Line, title, Title}|Rest], _Prefix) ->
    forms(Rest, Title);
forms([{function, _Line, test, 1, Clauses}|_Rest], Prefix) ->
    lists:filter(fun ({_, _, ignore}) -> false; (_) -> true end, signature_forms(Clauses, Prefix));
forms([_Form|Rest], Prefix) ->
    forms(Rest, Prefix).

signature_forms([], _Prefix) ->
    [];
signature_forms([{clause, Line, Head, _Guards, _Body}|Rest], Prefix) ->
    [{Line, Prefix, head(Head)}|signature_forms(Rest, Prefix)].


head([{string, _, "EOT"}]) ->
    ignore;
head([{string, _, String}]) ->
    String;
head([{tuple, _, [{string, _, String}]}]) ->
    String;
head([{tuple, _, [{string, _, String}, {cons, _, _, _}]}]) ->
    String.
