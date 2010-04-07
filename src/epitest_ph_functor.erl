-module(epitest_ph_functor).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{} = Test}, _From, State) ->
    Descriptor = normalize_implicit_functors(Test),
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({{start, #epistate{ worker = Worker } = Epistate}, #test{} = Test}, _From, State) ->
    spawn(fun () ->
                  Funs = epitest_property_helpers:functors(Test),
                  case (catch run_functors(Funs, Epistate)) of
                      {'EXIT', Reason} ->
                          gen_fsm:send_event(Worker, {failure, Reason});
                      _ ->
                          gen_fsm:send_event(Worker, success)
                  end
          end),
    {reply, {ok, Test}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.

normalize_implicit_functors(#test{ descriptor = Descriptor }) ->
    normalize_implicit_functors(Descriptor);
normalize_implicit_functors([F|Rest]) when is_function(F) ->
    [{functor, F}|normalize_implicit_functors(Rest)];
normalize_implicit_functors([Property|Rest]) ->
    [Property|normalize_implicit_functors(Rest)];
normalize_implicit_functors([]) ->
    [].

run_functors([F|Fs], Epistate) when is_function(F, 0) ->
    [F()|run_functors(Fs, Epistate)];
run_functors([F|Fs], Epistate) when is_function(F, 1) ->
    [F(Epistate)|run_functors(Fs, Epistate)];
run_functors([F|_Fs], #epistate{ test = Test }) when is_function(F) ->
    throw({badarity, Test, F});
run_functors([], _Epistate) ->
    [].

