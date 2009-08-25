-module(epitest_file_server).

-behaviour(gen_server).

-export([start_link/0]).


-export([redirect/2, cancel_redirection/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-define(SERVER, file_server_2).

start_link() -> 
    FileServerPid = whereis(?SERVER),
    unregister(?SERVER),
    register(original_file_server, FileServerPid),
    gen_server:start({local,?SERVER}, ?MODULE, [], []).

-record(state, { redirections }).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{ redirections = dict:new() }}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({redirect, Source, Prefix},  {_Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, ok, State#state{ redirections = dict:store(Source, Prefix, Redirections) }};

handle_call({cancel_redirection, Source},  {_Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, ok, State#state{ redirections = dict:erase(Source, Redirections) }};
    
handle_call({open, Name, ModeList}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State)  ->
    {reply, gen_server:call(original_file_server, {open, redirected_name(Name,Redirections,Pid), ModeList} ),  State};

handle_call({read_file, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {read_file, redirected_name(Name,Redirections,Pid)}), State};

handle_call({write_file, Name, Bin}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {write_file, redirected_name(Name,Redirections,Pid), Bin}), State};

handle_call({set_cwd, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {set_cwd, redirected_name(Name,Redirections,Pid)}), State};

handle_call({delete, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {delete, redirected_name(Name,Redirections,Pid)}), State};

handle_call({rename, Fr, To}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {rename, redirected_name(Fr,Redirections,Pid), redirected_name(To,Redirections,Pid)}), State};

handle_call({make_dir, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {make_dir, redirected_name(Name,Redirections,Pid)}), State};

handle_call({del_dir, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {del_dir, redirected_name(Name,Redirections,Pid)}), State};

handle_call({list_dir, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {list_dir, redirected_name(Name,Redirections,Pid)}), State};

handle_call(get_cwd, {_Pid, _Tag} = _From, #state{ redirections = _Redirections} = State) ->
    {reply, gen_server:call(original_file_server, get_cwd), State};

handle_call({get_cwd}, {_Pid, _Tag} = _From, #state{ redirections = _Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {get_cwd}), State};

handle_call({get_cwd, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {get_cwd, redirected_name(Name,Redirections,Pid)}), State};

handle_call({read_file_info, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {read_file_info, redirected_name(Name,Redirections,Pid)}), State};

handle_call({altname, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {altname, redirected_name(Name,Redirections,Pid)}), State};

handle_call({write_file_info, Name, Info}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {write_file_info, redirected_name(Name,Redirections,Pid), Info}), State};

handle_call({read_link_info, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {read_link_info, redirected_name(Name,Redirections,Pid)}), State};

handle_call({read_link, Name}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {read_link, redirected_name(Name,Redirections,Pid)}), State};

handle_call({make_link, Old, New}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {make_link, redirected_name(Old,Redirections,Pid), redirected_name(New,Redirections,Pid)}), State};

handle_call({make_symlink, Old, New}, {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {make_symlink, redirected_name(Old,Redirections,Pid), redirected_name(New,Redirections,Pid)}), State};

handle_call({copy, SourceName, SourceOpts, DestName, DestOpts, Length},
	    {Pid, _Tag} = _From, #state{ redirections = Redirections} = State) ->
    {reply, gen_server:call(original_file_server, {copy, redirected_name(SourceName,Redirections,Pid), SourceOpts, redirected_name(DestName,Redirections,Pid), DestOpts, Length}), State};

handle_call(stop, {_Pid, _Tag} = _From, #state{ redirections = _Redirections} = State) ->
    gen_server:call(original_file_server, stop),
    {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    error_logger:error_msg("handle_call(~p, ~p, _)", [Request, From]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:error_msg("handle_cast(~p, _)", [Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info(Info, State) ->
    original_file_server ! Info,
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
redirected_name([$/|_]=Name, _Redirections, _Pid) ->
    Name;
redirected_name(Name, Redirections, Pid) ->
    case dict:is_key(Pid, Redirections) of
	true ->
	    dict:fetch(Pid, Redirections) ++ "/" ++ Name;
	false ->
	    Name
    end.

%%%----------------------------------------------------------------------
%%% Public functions
%%%----------------------------------------------------------------------
redirect(Source, Prefix) ->
    gen_server:call(?SERVER, {redirect, Source, Prefix}).
cancel_redirection(Source) ->
    gen_server:call(?SERVER, {cancel_redirection, Source}).
