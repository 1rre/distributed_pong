-module(udp).
-behaviour(gen_server).
-export([start_link/2,init/1,handle_cast/2,handle_call/3]).

start_link(Parent, Port) ->
  gen_server:start_link(udp,[Parent, Port],[]).

init([Parent, Port]) ->
  {ok, Socket} = gen_udp:open(Port),
  ok = gen_udp:controlling_process(Socket, Parent),
  {ok, Socket}.


handle_call({recv, Msg}, From, Socket) ->
  io:fwrite("got: ~p from ~p~n", [Msg, From]),
  {reply, ok, Socket};

handle_call({send, Msg, Ip, Dest}, _From, Socket) ->
  io:fwrite("Sending: ~p to ~p~n", [Msg, {Ip, Dest}]),
  gen_udp:send(Socket, Ip, Dest, Msg),
  {reply, ok, Socket}.

handle_cast(_,State) -> {noreply, State}.

