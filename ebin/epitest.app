{application, epitest,
 [{description, "Erlang Parallel Integration Test"},
  {author, "yrashk@scallable.com"},
  {env, [{mods,
          [
           epitest_mod_require,
           epitest_mod_functor,
           epitest_mod_pending,
           epitest_mod_timetrap,
           epitest_mod_negative,
           epitest_mod_skip
          ]},
         {test_plan_handlers,
          [
           epitest_worker_notifier,
           epitest_console_logger,
           epitest_console_runner
          ]}
         ]},
         {timetrap_threshold, 30000},
  {mod, {epitest_app,[]}},
  {modules, [
             epitest,
             epitest_app,
             epitest_sup,
             epitest_test_server,
             epitest_beam,
             epitest_helpers,
             epitest_property_helpers,
             epitest_prophandler,
             epitest_prophandler_sup,
             epitest_mod_functor,
             epitest_mod_require,
             epitest_mod_negative,
             epitest_mod_pending,
             epitest_mod_timetrap,
             epitest_mod_skip,
             epitest_cluster,
             epitest_test_plan_server,
             epitest_test_plan_sup,
             epitest_test_worker,
             epitest_test_worker_sup,
             epitest_console_logger,
             epitest_console_runner,
             epitest_worker_notifier
            ]},
  {applications, [kernel, stdlib]}
 ]}.
