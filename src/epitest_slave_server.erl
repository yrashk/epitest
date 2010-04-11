-module(epitest_slave_server).
-behaviour(gen_server).


-include_lib("epitest/include/epitest.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
         }).


-define(SERVER, {?MODULE, epitest_cluster:name()}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{
      }}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
