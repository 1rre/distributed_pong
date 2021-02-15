-module(udp_sup).
-behaviour(application).
-export([start/2,stop/1,start_loop/0]).

-include("ports.hrl").

start(_Type, _Args) ->
  {ok, spawn(?MODULE, start_loop, [])}.

start_loop() ->
  {ok, A} = udp:start_link(self(), ?PORT_A),
  {ok, B} = udp:start_link(self(), ?PORT_B),
  gen_server:call(A, {send, "hello", ?IP, ?PORT_B}),
  loop(A,B).

loop(A,B) ->
  receive
    Msg -> io:fwrite("~p~n", [Msg])
  end,
  loop(A,B).

stop(_Type) -> ok.