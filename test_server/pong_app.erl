-module(pong_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_,_) ->
  supervisor:start_link({local,pong_sup},pong_sup,[]).

stop(_) -> ok.
