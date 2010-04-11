-module(epitest_mod_require).

-include_lib("epitest/include/epitest.hrl").
-export([init/1,handle_call/3]).

init(_) ->
    {ok, undefined}.

handle_call({normalize, #test{} = Test}, _From, State) ->
    %% TODO: Process shortcuts
    {reply, {ok, Test}, State};

handle_call({{plan, Plan}, #test{ loc = Loc } = Test}, From, State) ->
    spawn_link(fun () ->
                       Requirements = requirements(Test),
                       References = references(Requirements, Loc),
                       case References of
                           [] ->
                               ignore;
                           _ ->
                               gen_fsm:send_event(Plan, {load, References})
                       end,
                       gen_server:reply(From, {ok, Test})
               end),
    {noreply, State};

handle_call({{prepare, Plan}, #test{ id = ID, loc = Loc } = Test}, From, State) ->
    spawn_link(fun () ->
                       Requirements = requirements(Test),
                       Success = requirement_ids(success, Requirements, Loc),
                       Failure = requirement_ids(failure, Requirements, Loc),
                       Any = requirement_ids(any, Requirements, Loc),
                       All = Success ++ Failure ++ Any,
                       epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate) ->
                                                                                  Epistate#epistate {
                                                                                    mods_properties = [
                                                                                                           {requirements, All},
                                                                                                           {require_waiting_success, Success},
                                                                                                           {require_waiting_failure, Failure},
                                                                                                           {require_waiting_any, Any}|
                                                                                                           Epistate#epistate.mods_properties]
                                                                                   }
                                                                          end),
                       gen_server:reply(From, {ok, Test})
               end),
    {noreply, State};


handle_call({{start, #epistate{ 
                worker = Worker,
                mods_properties = Properties
               }}, #test{} = Test}, _From, State) ->
    case handle_start(proplists:get_value(require_waiting_success, Properties, []),
                      proplists:get_value(require_waiting_failure, Properties, []),
                      proplists:get_value(require_waiting_any, Properties, [])) of
        stop ->
            gen_fsm:send_event(Worker, stop),
            {reply, {stop, Test}, State};
        ok ->
            {reply, {ok, Test}, State}
    end;


handle_call({{notification, 
              #epistate{ 
                id = ID0,
                test_plan = Plan,
                variables = Variables,
                worker = Worker,
                mods_properties = Properties
               },
              #epistate{
                         id = ID,
                         state = Notification,
                         variables = VariablesNew
                       }
             }, #test{} = Test}, _From, State) ->
    %% Process variables
    case lists:member(ID, proplists:get_value(requirements, Properties, [])) of
        true ->
            epitest_test_plan_server:update_epistate(Plan, ID0, 
                                                     fun (Epistate) ->
                                                             Epistate#epistate {
                                                               variables = 
                                                               lists:ukeysort(1, lists:ukeymerge(1, 
                                                                                                 lists:ukeysort(1, VariablesNew),
                                                                                                 lists:ukeysort(1, Variables)))
                                                              }
                                                     end);
        false ->
            ignore
    end,
    %% Handle changes in requirement expectations
    case handle_notification(ID, Notification,
                             proplists:get_value(require_waiting_success, Properties, []),
                             proplists:get_value(require_waiting_failure, Properties, []),
                             proplists:get_value(require_waiting_any, Properties, [])) of
         {Success, Failure, Any} ->
            %% Update state
            epitest_test_plan_server:update_epistate(Plan, ID0, 
                                                     fun (Epistate) ->
                                                             Epistate#epistate {
                                                               mods_properties =
                                                               lists:ukeysort(1,
                                                                              [{require_waiting_success, Success},
                                                                               {require_waiting_failure, Failure},
                                                                               {require_waiting_any, Any}|
                                                                               Epistate#epistate.mods_properties])
                                                              }
                                                     end),
            gen_fsm:send_event(Worker, start); %% restart it
        {failed_requirement, {Req, ID}} ->
            FRTest = epitest_test_server:lookup(ID),
            gen_fsm:send_event(Worker, {failure, {failed_requirement, Req, FRTest} })
    end,
    {reply, {stop, Test}, State}.

%% Internal functions

handle_start(Success, Failure, Any) when (length(Success) > 0) orelse (length(Failure) > 0) orelse (length(Any) > 0) ->
    stop;
handle_start(_,_,_) ->
    ok.

handle_notification(ID, succeeded, Success0, Failure0, Any0) ->
    case lists:member(ID, Failure0) of
        true ->
            {failed_requirement, {failure, ID}};
        false ->
            Success = Success0 -- [ID],
            Failure = Failure0,
            Any = Any0 -- [ID],
            {Success, Failure, Any}
    end;

handle_notification(ID, {failed, _}, Success0, Failure0, Any0) ->
    case lists:member(ID, Success0) of
        true ->
            {failed_requirement, {success, ID}};
        false ->
            Success = Success0,
            Failure = Failure0 -- [ID],
            Any = Any0 -- [ID],
            {Success, Failure, Any}
    end.
                    

requirements(#test{ descriptor = Descriptor }) ->
    requirements(Descriptor);
requirements([{require, Reqs}|Rest]) ->
    lists:concat([Reqs,requirements(Rest)]);
requirements([_|Rest]) ->
    requirements(Rest);
requirements([]) ->
    [].

references([{success, Success}|Rest], Loc) ->
    lists:concat([load_references(Success, Loc),references(Rest, Loc)]);
references([{failure, Failure}|Rest], Loc) ->
    lists:concat([load_references(Failure, Loc),references(Rest, Loc)]);
references([{any, Any}|Rest], Loc) ->
    lists:concat([load_references(Any, Loc),references(Rest, Loc)]);
references([], _Loc) ->
    [].
    
load_references([T|L], Loc) ->
    lists:concat([query_references(T, Loc), load_references(L, Loc)]);
load_references([], _Loc) ->
    [].

-define(REFERENCE_QUERY(Match),
    epitest_test_server:q(fun (Test) ->
                                  case (catch Match = Test) of
                                      {'EXIT', _} -> 
                                          false;
                                      _ ->
                                          true
                                  end
                          end)).
        
query_references(Title, dynamic) ->
    ?REFERENCE_QUERY(#test{ loc = dynamic, signature = Title });
query_references({Module, Title}, dynamic) when is_atom(Module), is_list(Title) ->
    ?REFERENCE_QUERY(#test{ loc = {module, {Module, _}, _Line}, signature = Title});
query_references({Module, Title, Args}, dynamic) when is_atom(Module), is_list(Title), is_list(Args) ->
    ?REFERENCE_QUERY(#test{ loc = {module, {Module, _}, _Line}, signature = {Title, Args}});    
query_references(Title, {module, {Module, _}, _Line0}=Loc) when is_list(Title) ->
    query_references({Module, Title}, Loc);
query_references({Module, Title}, {module, {_Module0, _}, _Line0}) when is_atom(Module), is_list(Title) ->
    ?REFERENCE_QUERY(#test{ loc = {module, {Module, _}, _Line}, signature = Title});
query_references({Title, Args}, {module, {Module, _}, _Line0}=Loc) when is_list(Title), is_list(Args) ->
    query_references({Module, Title, Args}, Loc);
query_references({Module, Title, Args}, {module, {_Module0, _}, _Line0}) when is_atom(Module), is_list(Title), is_list(Args) ->
    case ?REFERENCE_QUERY(#test{ loc = {module, {Module, _}, _Line}, signature = {Title, Args}}) of
        [_|_] = Results -> %% Found something
            Results;
        [] ->
            {ok, Ref} = epitest_test_server:add({module, {Module, epitest_beam:prefix(Module)},
                                                 unknown}, %% That's Line. Can we actually find its line?
                                                {Title, Args}, 
                                                apply(Module, test, [{Title, Args}])),
            [epitest_test_server:lookup(Ref)]
    end;

query_references({Module, Title}, _Loc) when is_atom(Module) ->
    ?REFERENCE_QUERY(#test{ loc = {module, {Module, _}, _Line}, signature = Title}).

requirement_ids(Kind, [{Kind, Reqs}|Rest], Loc) ->
    Refs = load_references(Reqs, Loc),
    IDs = [ ID || #test{ id = ID } <- Refs ],
    lists:concat([IDs,requirement_ids(Kind, Rest, Loc)]);
requirement_ids(Kind, [_|Rest], Loc) ->
    requirement_ids(Kind, Rest, Loc);
requirement_ids(_Kind, [], _Loc) ->
    [].
    