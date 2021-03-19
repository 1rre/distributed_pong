-module(pong_app).
-behaviour(application).
-export([start/2,stop/1]).

% This file only exists as a boot script to start the supervisor.

start(_,_) ->
  % Start the supervisor
  supervisor:start_link({local,pong_sup},pong_sup,[]).

stop(_) -> ok.
