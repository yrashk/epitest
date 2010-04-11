-module(epitest_mod_negative).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({{start,  #epistate{ id = ID, test_plan = Plan }},
             #test{ descriptor = Descriptor } = Test}, _From, State) ->
    #epistate{ mods_properties = Properties } = epitest_test_plan_server:lookup(Plan, ID),
    Invert = proplists:get_value(negative, Descriptor, false),
    case Invert of 
        true ->
            Result = proplists:get_value(functor_result, Properties),
            epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate) ->
                                                                       Epistate#epistate{
                                                                         mods_properties = [{functor_result, 
                                                                                             invert(Result)}|
                                                                                            Epistate#epistate.mods_properties]
                                                                         }
                                                               end);
        _ ->
            ignore
    end,
    {reply, {ok, Test}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.


invert(success) ->
    {failure, negative};
invert({failure, _}) ->
    success.
