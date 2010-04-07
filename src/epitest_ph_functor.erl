-module(epitest_ph_functor).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{} = Test}, _From, State) ->
    Descriptor = normalize_implicit_functors(Test),
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({{start, #epistate{ id = ID, test_plan = Plan } = Epistate}, #test{} = Test}, From, State) ->
    spawn(fun () ->
                  Funs = functors(Test),
                  Result = 
                  case (catch run_functors(Funs, Epistate)) of
                      {'EXIT', Reason} ->
                          {failure, Reason};
                      _ ->
                          success
                  end,
                  epitest_test_plan_server:update_epistate(Plan, ID, 
                                                          fun (Epistate0) ->
                                                                  Epistate0#epistate{
                                                                    handlers_properties = [{functor_result, Result}|
                                                                                           Epistate0#epistate.handlers_properties]
                                                                   }
                                                          end),
                  gen_server:reply(From, {ok, Test})
          end),
    {noreply, State};

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

functors(#test{ descriptor = Descriptor }) ->
    functors(Descriptor);
functors([{functor, F}|Rest]) when is_function(F) ->
    [F|functors(Rest)];
functors([_Property|Rest]) ->
    functors(Rest);
functors([]) ->
    [].

run_functors([F|Fs], Epistate) when is_function(F, 0) ->
    [F()|run_functors(Fs, Epistate)];
run_functors([F|Fs], Epistate) when is_function(F, 1) ->
    [F(Epistate)|run_functors(Fs, Epistate)];
run_functors([F|_Fs], #epistate{ test = Test }) when is_function(F) ->
    throw({badarity, Test, F});
run_functors([], _Epistate) ->
    [].

