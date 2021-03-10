-module(rm_node).
-behaviour(application).
-export([start/2,stop/1,init/0]).

-record(state, {port, node}).

start(_,_) ->
  Pid = spawn(?MODULE, init, []),
  true = register(infoproc_cloud, Pid),
  {ok, Pid}.

stop(_) -> ok.

init() ->
  loop(#state{}).

loop(State) ->
  receive
    {Pid, Info} ->
      Msg = list_to_binary(io_lib:format("I recieved \"~s\" from you (~p)",[Info,Pid])),
      Pid!Msg,
      loop(State)
  end.
