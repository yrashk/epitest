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

handle_call({{start, Epistate}, #test{ descriptor = Descriptor } = Test}, _From, State) ->
    spawn(fun () ->
                  Funs =
                      lists:map(fun ({functor, Fun}) ->
                                        Fun
                                end,
                                lists:filter(fun ({functor, _Fun}) ->
                                                     true;
                                                 (_) ->
                                                     false
                                             end, Descriptor)),
                  lists:foreach(fun (Fun) ->
                                        if is_function(Fun, 0) ->
                                                Fun();
                                           is_function(Fun, 1) ->
                                                Fun(Epistate);
                                           true ->
                                                throw({badarity, Test, Fun})
                                        end
                                end, Funs)
          end),
    {reply, {ok, Test}, State}.
