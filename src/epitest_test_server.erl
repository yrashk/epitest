-module(epitest_test_server).
-behaviour(gen_server).

-export([add/2, lookup/1, load/1]).

-include_lib("epitest/include/epitest.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
					tests
				 }).

-define(SERVER, ?MODULE).

start_link() ->
		gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
		{ok, #state{
			 tests = ets:new(epitest_tests, [public])
			}}.

handle_call({add, Signature, Descriptor}, _From, #state{ tests = Tests } = State) ->
		% TODO: issue a warning
		ets:insert(Tests, {Signature, Descriptor}),
		{reply, ok, State};

handle_call({lookup, Signature}, _From, #state{ tests = Tests } = State) ->
		case ets:lookup(Tests, Signature) of
				[] ->
						{reply, {error, notfound}, State};
				[Descriptor] ->
						{reply, Descriptor, State}
		end;

handle_call({load, Module}, From, State) ->
		spawn_link(fun () ->
											 Signatures = epitest_beam:signatures(Module),
											 [ add(Signature, apply(Module, test, [Signature])) || Signature <- Signatures ],
											 gen_server:reply(From, ok)
							 end),
		{noreply, State}.

handle_cast(_Msg, State) ->
		{noreply, State}.

handle_info(_Info, State) ->
		{noreply, State}.

terminate(_Reason, _State) ->
		ok.

code_change(_OldVsn, State, _Extra) ->
		{ok, State}.

%% Public functions
-spec add(test_signature(), test_descriptor()) -> 'ok' | {'error', any()}.
								  
add(Signature, Descriptor) ->
		gen_server:call({global, ?SERVER}, {add, Signature, Descriptor}).

-spec lookup(test_signature()) -> test_descriptor() | {'error', any()}.
										 
lookup(Signature) ->
		gen_server:call({global, ?SERVER}, {lookup, Signature}).

-spec load(module()) -> 'ok' | {'error', any()}.

load(Module) ->
		gen_server:call({global, ?SERVER}, {load, Module}).
									 
