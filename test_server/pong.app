{application, pong,
 [{description, "the remote node"},
  {vsn, "0.1.0"},
  {modules, [pong_app,pong_sup,pong_game,pong_server]},
  {registered, [pong_game,pong_server]},
  {mod, {pong_app, []}},
  {applications, [kernel, stdlib]},
  {env, []}
]}.
