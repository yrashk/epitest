-module(epitest_mod_functor).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({normalize, #test{} = Test}, _From, State) ->
    Descriptor = normalize_implicit_functors(Test),
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({{start, #epistate{ id = ID, test_plan = Plan } = Epistate}, #test{} = Test}, From, State) ->
    spawn(fun () ->
                  Funs = epitest_property_helpers:functors(Test),
                  {Elapsed, Result} = 
                  case (catch timer:tc(epitest_property_helpers,run_functors,[Funs, Epistate])) of
                      {ElapsedMs, {'EXIT', Reason}} ->
                          {ElapsedMs, {failure, Reason}};
                      {ElapsedMs, _Result} ->
                          {ElapsedMs, success}
                  end,
                  epitest_test_plan_server:update_epistate(Plan, ID, 
                                                          fun (Epistate0) ->
                                                                  Epistate0#epistate{
                                                                    elapsed = Elapsed,
                                                                    mods_properties = [{functor_result, Result}|
                                                                                       Epistate0#epistate.mods_properties]
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

