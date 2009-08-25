-module(epitest_file_server).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-define(SERVER, file_server_2).

start_link() -> 
    FileServerPid = whereis(?SERVER),
    unregister(?SERVER),
    register(original_file_server, FileServerPid),
    gen_server:start({local,?SERVER}, ?MODULE, [FileServerPid], []).

-record(state, { file_server }).

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
init([FileServerPid]) ->
    {ok, #state{ file_server = FileServerPid }}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({open, Name, ModeList}, _From, #state{ file_server = Server} = State)  ->
    {reply, gen_server:call(Server, {open, transform_name(Name), ModeList} ),  State};

handle_call({read_file, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {read_file, transform_name(Name)}), State};

handle_call({write_file, Name, Bin}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {write_file, transform_name(Name), Bin}), State};

handle_call({set_cwd, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {set_cwd, transform_name(Name)}), State};

handle_call({delete, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {delete, transform_name(Name)}), State};

handle_call({rename, Fr, To}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {rename, transform_name(Fr), transform_name(To)}), State};

handle_call({make_dir, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {make_dir, transform_name(Name)}), State};

handle_call({del_dir, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {del_dir, transform_name(Name)}), State};

handle_call({list_dir, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {list_dir, transform_name(Name)}), State};

handle_call(get_cwd, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, get_cwd), State};

handle_call({get_cwd}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {get_cwd}), State};

handle_call({get_cwd, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {get_cwd, transform_name(Name)}), State};

handle_call({read_file_info, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {read_file_info, transform_name(Name)}), State};

handle_call({altname, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {altname, transform_name(Name)}), State};

handle_call({write_file_info, Name, Info}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {write_file_info, transform_name(Name), Info}), State};

handle_call({read_link_info, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {read_link_info, transform_name(Name)}), State};

handle_call({read_link, Name}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {read_link, transform_name(Name)}), State};

handle_call({make_link, Old, New}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {make_link, transform_name(Old), transform_name(New)}), State};

handle_call({make_symlink, Old, New}, _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {make_symlink, transform_name(Old), transform_name(New)}), State};

handle_call({copy, SourceName, SourceOpts, DestName, DestOpts, Length},
	    _From, #state{ file_server = Server} = State) ->
    {reply, gen_server:call(Server, {copy, transform_name(SourceName), SourceOpts, transform_name(DestName), DestOpts, Length}), State};

handle_call(stop, _From, #state{ file_server = Server} = State) ->
    gen_server:call(Server, stop),
    {stop, normal, stopped, State};

handle_call(Request, From, #state{ file_server = _Server} = State) ->
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

handle_info(Info, #state{ file_server = Server } = State) ->
    Server ! Info,
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
transform_name(Name) ->
    Name.
