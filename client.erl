-module(client).
-export([main/1]).

-define(IP_ADDRESS, {35,234,158,230}).
-define(PORT, 4000).
 
main(_Args) ->
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, []),
  loop(Socket).
 
loop(Socket) ->
  Message = lists:droplast(io:get_line("Enter a message to send to the server ~> ")),
  if
    Message =:= ":q!" -> 
      gen_tcp:send(Socket, Message),
      halt(0);
    true ->
      gen_tcp:send(Socket, Message),
      loop(Socket)
  end.
