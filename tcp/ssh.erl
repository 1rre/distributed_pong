-module(start_ssh).
-export([main/1]).
-mode(compile).
-define(IP, {34,105,136,143}).
-define(Port, 22).

main(_Args) ->
  ssh:start(),
  {ok, Port} = ssh:connect(?IP, ?Port, [{connect_timeout, 5000}, {user, "tmoores"}]),
  ssh_connection:exec(Port, open_channel(Port), "escript", infinity),
  case catch_exit() of
    1 -> ok;
    _ -> do_pkg_update(Port)
  end,
  send_file(Port),
  ssh_connection:exec(Port, open_channel(Port), "escript server.erl", infinity),
  echo_messages().

do_pkg_update(Port) ->
  ssh_connection:exec(Port, open_channel(Port), "sudo apt-get -y update", infinity),
  case catch_exit() of
    0 -> ok;
    Err_Update -> halt(Err_Update)
  end,
  ssh_connection:exec(Port, open_channel(Port), "sudo apt-get -y install erlang", infinity),
  case catch_exit() of
    0 -> ok;
    Err_Install -> halt(Err_Install)
  end.

send_file(Port) ->
  {ok, Server_Code} = file:read_file("server.erl"),
  {ok, Channel} = ssh_sftp:start_channel(Port),
  ok = ssh_sftp:write_file(Channel, "server.erl", Server_Code),
  ssh_sftp:stop_channel(Channel).


echo_messages() -> receive 
  {ssh_cm,_Pid,{data,_,_,Msg}} ->
    io:fwrite("~s", [Msg]),
    echo_messages();
  {ssh_cm,_Pid,{exit_status,_,N}} -> halt(N);
  _ -> echo_messages()
end.

open_channel(Port) -> 
  {ok, Channel} = ssh_connection:session_channel(Port, 5000),
  Channel.

catch_exit() -> receive
  {ssh_cm,_Pid,{exit_status,_,N}} -> N;
  _Msg -> catch_exit()
end.

