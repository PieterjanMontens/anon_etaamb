{application, server,
 [{description, "Anon server"},
  {vsn, "0.1.0"},
  {modules, [server_app,
             server_sup
            ]},
  {registered, [server_sup]},
  {applications, [kernel, stdlib]},
  {mod, {server_app, []}}
 ]}.
