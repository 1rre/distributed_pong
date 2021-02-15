-module(udp_sup).
-behaviour(application).
-export([start/2,stop/1,start_loop/0]).

-include("ports.hrl").

start(_Type, _Args) ->
  {ok, spawn(?MODULE, start_loop, [])}.

start_loop() ->
  erlang:register(udp_sup, self()),
  {ok, Socket} = udp:start_link(self(), ?PORT_A),
  loop(Socket).

loop(Socket) ->
  receive
    {send,Msg} -> gen_server:cast(Socket, {send, Msg, ?IP, ?PORT_B});
    {udp,_Sock,_Ip,_Port,Msg} -> gen_server:cast(Socket, {recv, Msg});
    Msg -> io:fwrite("Received ~p~n", [Msg])
  end,
  loop(Socket).

stop(_Type) -> ok.
