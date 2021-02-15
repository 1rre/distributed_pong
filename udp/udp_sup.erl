-module(udp_sup).
-behaviour(application).
-export([start/2,stop/1,start_loop/0]).

-include("ports.hrl").

start(_Type, _Args) ->
  {ok, spawn(?MODULE, start_loop, [])}.

start_loop() ->
  erlang:register(udp_sup, self()),
  {ok, A} = udp:start_link(self(), ?PORT_A),
  {ok, B} = udp:start_link(self(), ?PORT_B),
  loop(A,B).

loop(A,B) ->
  receive
    {send,a,Msg} -> gen_server:cast(A, {send, Msg, ?IP, ?PORT_B});
    {send,b,Msg} -> gen_server:cast(B, {send, Msg, ?IP, ?PORT_A});
    {udp,_Port,_Ip,?PORT_A,Msg} -> gen_server:cast(A, {recv, Msg});
    {udp,_Port,_Ip,?PORT_B,Msg} -> gen_server:cast(B, {recv, Msg});
    Msg -> io:fwrite("Received ~p~n", [Msg])
  end,
  loop(A,B).

stop(_Type) -> ok.
