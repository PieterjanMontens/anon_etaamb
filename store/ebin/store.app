% simple key/value store
{application, store,
 [{description, "A simple key/value store"},
  {vsn, "0.1.0"},
  {modules, [store_app,
             store_sup
            ]},
  {registered, [store_sup]},
  {applications, [kernel, stdlib]},
  {mod, {store_app, []}}
 ]}.
