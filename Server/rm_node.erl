-module(rm_node).
-behaviour(application).
-export([start/2,stop/1,init/0]).

start(_,_) ->
  Pid = spawn(?MODULE, init, []),
  true = register(infoproc_cloud, Pid),
  {ok, Pid}.

stop(_) -> ok.

init() -> loop(#{}).

loop(State) ->
  receive
    {nodedown,Node} ->
      Pid = maps:get(Node,State),
      N_State = maps:remove(Node,State),
      Msg_Str = io_lib:format("Process ~p on node ~s has gone down",[Pid,Node]),
      Msg = list_to_binary(Msg_Str),
      [O_Pid!Msg||O_Pid<-maps:values(N_State)],
      loop(N_State);

    {Pid,{register,Node}} ->
      N_Board_Str = io_lib:format("Registered ~p (you) as on node ~s",[Pid,Node]),
      N_Board_Msg = list_to_binary(N_Board_Str),
      Pid!N_Board_Msg,
      Board_Str = io_lib:format("New process registered: ~p on node ~s",[Pid,Node]),
      Board_Msg = list_to_binary(Board_Str),
      [O_Pid!Board_Msg||O_Pid<-maps:values(State)],
      monitor_node(Node,true),
      loop(maps:put(Node,Pid,State));
    {Node,{led,on}} ->
      io:fwrite("leds on~n"),
      case maps:get(Node,State,undefined) of
        undefined -> loop(State);
        Pid ->
          Msg = {led,on},
          [P!Msg||P<-maps:values(State)],%,P =/= Pid],
          loop(State)
      end;
    {Node,{led,off}} ->
      io:fwrite("leds off~n"),
      case maps:get(Node,State,undefined) of
        undefined -> loop(State);
        Pid ->
          Msg = {led,on},
          [P!Msg||P<-maps:values(State)],%,P =/= Pid],
          loop(State)
      end;
    Msg -> io:fwrite("~p~n",[Msg]),
           loop(State)
  end.
    
