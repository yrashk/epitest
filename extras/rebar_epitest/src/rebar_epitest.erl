-module(rebar_epitest).
-export([epitest/2, clean/2]).

epitest(Config, _File) ->
    {ok, Cwd} = file:get_cwd(),
    ok = filelib:ensure_dir(Cwd ++ "/t"),

    TestErls = rebar_utils:find_files("t", ".*\\.erl\$"),
    rebar_erlc_compiler:doterl_compile(epitest_config(Config), "t", TestErls),
	
    Modules = [list_to_atom(filename:basename(N, ".beam")) ||
                  N <- filelib:wildcard("*.beam", "t")],

    code:add_path("t"),
    code:add_path(rebar_config:get_list(Config, epitest_ebin_dir, "ebin")), %% FIXME: make it nicer
    case erl_epmd:names() of
         {error, address} ->
             os:cmd("epmd &");
         _ -> ignore
    end,
    net_kernel:start([test, shortnames]),
    epitest:start(),
    application:set_env(epitest, console_runner_master_proc, self()),

    [ {ok, _} = epitest_test_server:load(Module) || Module <- Modules ],
    {ok, _Pid} = supervisor:start_child(epitest_test_plan_sup, ["Default", 
                                                                fun (_) -> true end]),
    receive
        epitest_console_plans_finished ->
            ok;
        {epitest_console_plan_finished, _Plan} ->
            ok
    end,
    net_kernel:stop(),
    ok.

clean(_Config, _File) ->
    [] = os:cmd("rm -f t/*.beam"),
    ok.



%%% Internal functions


epitest_config(Config) ->
    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EpitestOpts = rebar_config:get_list(Config, epitest_compile_opts, []),
    Opts = [debug_info] ++ EpitestOpts ++ ErlOpts,
    rebar_config:set(Config, erl_opts, Opts).

