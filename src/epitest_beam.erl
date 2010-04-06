-module(epitest_beam).
-export([signatures/1, prefix/1]).
-include_lib("epitest/include/epitest.hrl").


-spec prefix(module()) -> string() | 'undefined'.
                     
prefix(Module) ->
    {Module, ModuleBinary, _Filename} = code:get_object_code(Module),
    case beam_lib:chunks(ModuleBinary, [abstract_code]) of
        {ok, {_, [{abstract_code, {_, AC}}]}} ->
            find_prefix(AC);
        {error, beam_lib, _Reason} ->
	    % TODO: issue a warning or actually handle errors everywhere where we use this
            []
    end.

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

find_prefix([]) ->
    undefined;
find_prefix([{attribute, _Line, title, Title}|_Rest]) ->
    Title;
find_prefix([_Form|Rest]) ->
    find_prefix(Rest).

forms(L) ->
    forms(L, undefined).

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
head([{tuple, _, [{string, _, _String}, {cons, _, _, _}]}]) ->
    ignore. %% We can't get its descriptor anyway (this test is parametrized)
