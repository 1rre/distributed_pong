-define(PORT, 4000).
 
main(_Args) ->
  {ok, Listen} = gen_tcp:listen(?PORT, [{keepalive, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket).
 
loop(Socket) ->
  receive
    {tcp, _From, Received} ->
      io:fwrite("Received: ~s~n", [Received]),
      loop(Socket);
    _ -> halt(0)
  end.
