-module(epitest_slave).
-export([start/0]).

get_path() ->
    lists:map(fun filename:absname/1, code:get_path()).    

start() ->
    {ok, Host} = inet:gethostname(),
    {SNodename, Nodename} = generate_nodename(),
    {ok, Cwd} = file:get_cwd(),
    Dir = proplists:get_value(dir, application:get_all_env(epitest), "."),
    NewDir = Cwd ++ "/" ++ Dir ++ "/" ++ SNodename,
    ok = file:make_dir(NewDir),
    Paths = get_path(),
    {ok, Node} = slave:start(list_to_atom(Host), Nodename, []),
    ok = rpc:call(Node, file, set_cwd, [NewDir]),
    ok = rpc:call(Node, code, add_paths, [Paths]),
    {ok, Node}.

generate_nodename() ->
    {A,B,C} = now(), 
    S = erlang:integer_to_list(A+B+C,16),
    {S, erlang:list_to_atom(S)}.
       
