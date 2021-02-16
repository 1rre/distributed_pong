-module(client).
-export([main/1]).

-define(IP_ADDRESS, {34,105,136,143}).
-define(PORT, 4000).
 
main(_Args) ->
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, [{active, true}, {keepalive, true}]),
  loop(Socket).
 
loop(Socket) ->
  Message = lists:droplast(io:get_line("Enter a message to send to the server ~> ")),
  if
    Message =:= ":q!" -> 
      ok = gen_tcp:send(Socket, Message),
      ok = gen_tcp:close(Socket),
      halt(0);
    true ->
      ok = gen_tcp:send(Socket, Message),
      loop(Socket)
  end.
