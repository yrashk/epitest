-module(epitest_ph_require).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{} = Test}, _From, State) ->
    %% TODO: Process shortcuts
    {reply, {ok, Test}, State};

handle_call({{plan, Plan}, #test{ loc = Loc } = Test}, _From, State) ->
    Requirements = requirements(Test),
    References = references(Requirements, Loc),
    case References of
        [] ->
            ignore;
        _ ->
            gen_fsm:send_event(Plan, {load, References})
    end,
    {reply, {ok, Test}, State};

handle_call({{prepare, Plan}, #test{ id = ID, loc = Loc } = Test}, _From, State) ->
    Requirements = requirements(Test),
    Success = requirement_ids(success, Requirements, Loc),
    Failure = requirement_ids(failure, Requirements, Loc),
    Any = requirement_ids(any, Requirements, Loc),
    epitest_test_plan_server:update_epistate(Plan, ID, fun (Epistate) ->
                                                              Epistate#epistate {
                                                                handlers_properties = [{require_waiting_succcess, Success},
                                                                                       {require_waiting_failure, Failure},
                                                                                       {require_waiting_any, Any}|
                                                                                       Epistate#epistate.handlers_properties]
                                                               }
                                                      end),
    {reply, {ok, Test}, State};

handle_call({{start, _Worker, _Epistate}, #test{} = Test}, _From, State) ->
    {reply, {ok, Test}, State}.

%% Internal functions

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
    ?REFERENCE_QUERY(#test{ signature = Title });
query_references(Title, {module, Module, _Line0}) ->
    ?REFERENCE_QUERY(#test{ loc = {module, Module, _Line}, signature = Title});
query_references({Module, Title}, _Loc) when is_atom(Module) ->
    ?REFERENCE_QUERY(#test{ loc = {module, Module, _Line}, signature = Title}).

requirement_ids(Kind, [{Kind, Reqs}|Rest], Loc) ->
    Refs = load_references(Reqs, Loc),
    IDs = [ ID || #test{ id = ID } <- Refs ],
    lists:concat([IDs,requirement_ids(Kind, Rest, Loc)]);
requirement_ids(Kind, [_|Rest], Loc) ->
    requirement_ids(Kind, Rest, Loc);
requirement_ids(_Kind, [], _Loc) ->
    [].
    
