-module(client).
-export([main/1]).

%-define(IP_ADDRESS, {35,234,158,230}).
-define(IP_ADDRESS, localhost).
-define(PORT, 4000).

main(_Args) ->
  io:fwrite("Connecting to port ~B on ~p~n", [?PORT, ?IP_ADDRESS]),
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, [{active, true}, {keepalive, true}]),
  io:fwrite("Connected: ~p~n", [Socket]),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello"),
  gen_tcp:send(Socket, "Hello").

