% anoner OTP .app file
{application, anoner,
 [{description, "A text anonymising project"},
  {vsn, "0.1.0"},
  {modules, [anon_app,
             anon_top
            ]},
  {registered, [anon_top]},
  {applications, [kernel, stdlib]},
  {mod, {anon_app, []}}
 ]}.
