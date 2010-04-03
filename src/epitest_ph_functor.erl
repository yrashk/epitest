-module(epitest_ph_functor).

-include_lib("epitest/include/epitest.hrl").
-export([init/0,handle_call/3]).

init() ->
    {ok, undefined}.

handle_call({uniform, #test{ descriptor = Descriptor0 } = Test}, _From, State) ->
    Descriptor =
        lists:map(fun (F) when is_function(F) ->
                          {functor, F};
                      (Other) ->
                          Other
                  end, Descriptor0),
    {reply, {ok, Test#test{ descriptor = Descriptor }}, State}.
