-module(epitest).
-vsn(?vsn).

-export([start/0,stop/0]).

-export([parse_transform/2]).

start() ->
    application:start(epitest).

stop() ->
    application:stop(epitest).

%%% PARSE TRANSFORMATION

parse_transform(Forms, _Options) ->
    forms(Forms).


forms([{function, L, N, A, Cs}|Fs]) ->
    [rewrite_function(L,N,A,Cs)|forms(Fs)];
forms([F|Fs]) ->
    [F|forms(Fs)];
forms([]) ->
    [].


rewrite_function(Line, Name, Arity, Clauses) ->
    Clauses1 = clauses(Clauses),
    {function, Line, Name,Arity,Clauses1}.

clauses([C|Cs]) ->
    {clause, L,H,G,B} = C,
    B1 = body(B),
    [{clause,L,H,G, B1}|Cs];
clauses([]) ->
    [].

body([E|Es]) ->
    [expr(E)|body(Es)];
body([]) ->
    [].

expr({call, Line, {atom,_,epitest_wait}, As}) ->
    {call, Line, {atom,Line,wait_handler}, As};
expr(E) ->
    E.
