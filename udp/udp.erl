-module(udp).
-behaviour(gen_server).
-export([start_link/2,init/1,handle_cast/2,handle_call/3]).

start_link(Parent, Port) ->
  gen_server:start_link(udp,[Parent, Port],[]).

init([Parent, Port]) ->
  {ok, Socket} = gen_udp:open(Port),
  ok = gen_udp:controlling_process(Socket, Parent),
  {ok, Socket}.


handle_cast({recv, Msg}, Socket) ->
  io:fwrite("received: ~p~n", [Msg]),
  {noreply, Socket};

handle_cast({send, Msg, Ip, Dest}, Socket) ->
  io:fwrite("Sending: ~p to ~p~n", [Msg, {Ip, Dest}]),
  gen_udp:send(Socket, Ip, Dest, Msg),
  {noreply, Socket};
handle_cast(Msg, Socket) ->
  io:fwrite("received: ~p~n", [Msg]),
  {noreply, Socket}.

handle_call(_,_,State) -> {reply, ok, State}.

