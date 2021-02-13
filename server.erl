-module(server).
-export([main/1]).

-define(PORT, 4000).

main(_Args) ->
  {ok, Listen}= gen_tcp:listen(?PORT, [{keepalive, true}, {active, true}]),
  {ok, Port_Num} = inet:port(Listen),
  io:fwrite("Port is: ~B~n", [Port_Num]),
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket).

loop(Socket) ->
  receive
    Recieved -> 
      io:fwrite("Recieved:~n~p~n", [Recieved])
  end,
  loop(Socket).

