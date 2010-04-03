-module(epitest_ph_functor).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({normalize, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor =
        lists:map(fun (F) when is_function(F) ->
                          {functor, F};
                      (Other) ->
                          Other
                  end, Descriptor0),
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State};

handle_call({start, #test{ descriptor = Descriptor } = Test}, _From, State) ->
    Funs =
        lists:map(fun ({functor, Fun}) ->
                          Fun
                  end,
                  lists:filter(fun ({functor, Fun}) ->
                                       true;
                                   (_) ->
                                       false
                               end, Descriptor)),
    Test.
