{application, epitest,
 [{description, "Erlang Parallel Integration Test"},
  {author, "yrashk@scallable.com"},
  {env, [{property_handlers,
          [epitest_ph_functor]
         }]},
  {mod, {epitest_app,[]}},
  {modules, [
             epitest,
             epitest_app,
             epitest_sup,
             epitest_test_server,
             epitest_beam,
             epitest_prophandler,
             epitest_prophandler_sup,
             epitest_ph_functor
            ]},
  {applications, [kernel, stdlib]}
 ]}.
