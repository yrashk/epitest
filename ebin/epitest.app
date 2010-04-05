{application, epitest,
 [{description, "Erlang Parallel Integration Test"},
  {author, "yrashk@scallable.com"},
  {env, [{property_handlers,
          [
           epitest_ph_require,
           epitest_ph_functor
          ]},
         {test_plan_handlers,
          [
           epitest_worker_notifier,
           epitest_console_logger,
           epitest_console_runner
          ]}
         ]},
  {mod, {epitest_app,[]}},
  {modules, [
             epitest,
             epitest_app,
             epitest_sup,
             epitest_test_server,
             epitest_beam,
             epitest_helpers,
             epitest_prophandler,
             epitest_prophandler_sup,
             epitest_ph_functor,
             epitest_ph_require,
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
