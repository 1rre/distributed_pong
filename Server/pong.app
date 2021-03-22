% Sets out the information for the pong app
{application, pong,
 [{description, "Hey kiddo, you like pong?"},
  % Version number (I've basically ignored this because it only affects professional stuff)
  {vsn, "0.1.0"},
  % The modules which this app is contained in
  {modules, [pong_app,pong_sup,pong_game,pong_server]},
  % The registered names for this app (to avoid conflicts)
  {registered, [pong_game,pong_server]},
  % The root module of this app
  {mod, {pong_app, []}},
  % The dependencies of this app (the kernel & standard library)
  {applications, [kernel, stdlib]},
  % idk environment variables or something? google it.
  {env, []}
]}.
