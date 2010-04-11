-module(epitest_mod_nodesplit).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0} = Test}, _From, State) ->
    case proplists:get_value(nodesplit, Descriptor0) of
        undefined ->
            {reply, {ok, Test}, State};
        _ ->
            Funs = epitest_property_helpers:functors(Test),
            Fun = rewrite_funs(Funs),
            Descriptor = [Fun|epitest_property_helpers:remove_functors(Descriptor0)],
            {reply, {ok, Test#test{ descriptor = Descriptor }}, State}
    end;


handle_call({{prepare, Plan}, #test{ descriptor = Descriptor } = Test}, _From, State) ->
    case proplists:get_value(nodesplit, Descriptor) of
        undefined ->
            ignore;
        true ->
            prepare_nodesplit(Plan, Test, []);
        PropList when is_list(PropList) ->
            prepare_nodesplit(Plan, Test, PropList)
    end,
    {reply, {ok, ignore}, State};
    
handle_call({{notification, 
              #epistate{ 
                id = ID,
                test_plan = Plan,
                test = Test,
                mods_properties = Properties
               },
              #epistate{
                         id = ID0,
                         mods_properties = Properties0
                       }
             }, #test{} = Test}, _From, State) ->
    case lists:member(ID0, proplists:get_value(requirements, Properties, [])) of
        true ->
            case proplists:get_value(splitnode, Properties0) of
                undefined ->
                    ignore;
                Splitnode ->
                    #test{ descriptor = Descriptor0 } = Test,
                    Funs = epitest_property_helpers:functors(Test),
                    Fun = rewrite_funs(Funs),
                    Descriptor = [{functor, Fun}|epitest_property_helpers:remove_functors(Descriptor0)],
                    epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate) ->
                                                                               Epistate#epistate{
                                                                                 test = Test#test { descriptor = Descriptor },
                                                                                 mods_properties = [{splitnode, Splitnode}|
                                                                                                    Epistate#epistate.mods_properties]
                                                                                }
                                                                       end)
            end;
        _ ->
            ignore
    end,
    {reply, {ok, Test}, State};

handle_call({_Message, Result}, _From, State) ->
    {reply, {ok, Result}, State}.


%% Internal functions
prepare_nodesplit(Plan, #test{ id = ID }, _Properties) ->
    Ref = epitest_slave_server:create(),
    epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate) ->
                                                               Epistate#epistate{
                                                                 mods_properties = [{splitnode, Ref}|
                                                                                    Epistate#epistate.mods_properties]
                                                                 }
                                                       end),
    ok.

rewrite_funs(Fs) ->
    fun (#epistate{ test_plan = Plan, id = ID, mods_properties = Properties } = Epistate) ->
            Splitnode = proplists:get_value(splitnode, Properties),
            {ok, Node} = epitest_slave_server:ensure_started(Splitnode),
            epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate0) ->
                                                                       Epistate0#epistate{
                                                                         node = Node
                                                                        }
                                                               end),
            {ok, Result} = epitest_slave_server:eval(Splitnode, fun () ->
                                                                        epitest_property_helpers:run_functors(Fs, Epistate) 
                                                                end),
            Result
    end.


