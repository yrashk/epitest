{application, epitest,
 [{description, "Erlang Parallel Integration Test"},
  {author, "yrashk@scallable.com"},
  {mod, {epitest_app,[]}},
  {modules, [
             epitest,
             epitest_app,
             epitest_sup,
						 epitest_test_server,
						 epitest_beam
            ]},
  {applications, [kernel, stdlib]}
  ]}.
