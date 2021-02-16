-module(server).
-export([main/1]).

-define(PORT, 4000).
 
main(_Args) ->
  {ok, Listen} = gen_tcp:listen(?PORT, [{active, true}, {keepalive, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket).
 
loop(Socket) ->
  receive
    {tcp, _From, ":q!"} ->
      ok = gen_tcp:close(Socket),
      halt(0);
    {tcp, _From, Received} ->
      io:fwrite("Received: ~s~n", [Received]),
      loop(Socket);
    _ ->
      ok = gen_tcp:close(Socket),
      halt(0)
  end.
