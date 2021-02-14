-module(start_ssh).
-export([main/1]).
%34.105.136.143
-define(IP, {34,105,136,143}).
-define(Port, 22).

main(_Args) ->
  ssh:start(),
  {ok, Port} = ssh:connect(?IP, ?Port, [{connect_timeout, 5000}, {user, "tmoores"}, {keepalive, true}]),
  {ok, Channel} = ssh_connection:session_channel(Port, 5000),
  %success = ssh_connection:exec(Port, Channel, "bash", infinity),
  ok = ssh_connection:shell(Port, Channel),
  ssh_connection:send(Port, Channel, "escript 2> /dev/null\necho $?\n"),
  check_erl_inst(Port, Channel),
  ok.

check_erl_inst(Port, Channel) ->
  receive
    {ssh_cm, _Pid, {data,0,0,<<"127\n">>}} -> 
      io:fwrite("Installing Erlang~n"),
      ssh_connection:send(Port, Channel, "sudo apt-get -y update 2> /dev/null &&"),
      ssh_connection:send(Port, Channel, "sudo apt-get -y install erlang erlang-dev 2>/dev/null\n"),
      check_server_inst(Port, Channel);
    {ssh_cm, _Pid, {data,0,0,<<"1\n">>}} -> 
      io:fwrite("Erlang installed~n"),
      ssh_connection:send(Port, Channel, "cd Info_Proc_Labs 2> /dev/null; echo $?\n"),
      check_server_inst(Port, Channel);
    Msg -> 
      io:fwrite("Got: ~n~p~n", [Msg]),
      check_erl_inst(Port, Channel)
  end.

check_server_inst(Port, Channel) ->
  io:fwrite("Checking server inst~n"),
  receive
    {ssh_cm, _Pid, {data,0,0,<<"0\n">>}} -> 
      io:fwrite("Dir exits~n"),
      run_server(Port, Channel);  
    {ssh_cm, _Pid, {data,0,0,<<"1\n">>}} -> 
      io:fwrite("Cloning~n"),
      do_git_clone(Port, Channel);  
    _Msg -> check_server_inst(Port, Channel)
  end.

do_git_clone(Port, Channel) -> 
  ssh_connection:send(Port, Channel, "git clone https://github.com/tjm1518/Info_Proc_Labs.git 2>/dev/null && cd Info_Proc_Labs\n"),
  run_server(Port, Channel).

run_server(Port, Channel) -> 
  io:fwrite("Running Server\n"),
  ssh_connection:exec(Port, Channel, "screen escript server.erl", infinity),
  loop().


loop() ->
  receive 
    Msg -> io:fwrite("Got ~n~p~n", [Msg])
  end,
  loop().

