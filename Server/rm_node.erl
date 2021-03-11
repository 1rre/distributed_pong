-module(rm_node).
-behaviour(application).
-export([start/2,stop/1,loop/1]).

% Start the application by spawning a process to recieve messages
% & make it visible to anyone who connects to the node

start(_,_) ->
  Pid = spawn(?MODULE, loop, [#{}]),
  true = register(infoproc_cloud, Pid),
  {ok, Pid}.

% Don't do anything special to exit the application

stop(_) -> ok.

% Loop to receive messages & handle them

loop(State) ->
  receive
    % A node has disconnected
    {nodedown,Node} ->
      % Send a message to all the other registered nodes saying that the node has disconnected
      Pid = maps:get(Node,State),
      Msg_Str = io_lib:format("Process ~p on node ~s has gone down",[Pid,Node]),
      io:fwrite("~s~n",[Msg_Str]),
      Msg = list_to_binary(Msg_Str),
      N_State = maps:remove(Node,State),
      [O_Pid!Msg||O_Pid<-maps:values(N_State)],
      loop(N_State);
    % A new node has connected
    {Pid,{register,Node}} ->
      % Send a message to the new node telling them that they've registered
      N_Board_Str = io_lib:format("Registered ~p (you) as on node ~s",[Pid,Node]),
      N_Board_Msg = list_to_binary(N_Board_Str),
      Pid!N_Board_Msg,
      % Send a message to everyone else telling them that another board has registered
      Board_Str = io_lib:format("New process registered: ~p on node ~s",[Pid,Node]),
      io:fwrite("~s~n",[Board_Str]),
      Board_Msg = list_to_binary(Board_Str),
      [O_Pid!Board_Msg||O_Pid<-maps:values(State)],
      monitor_node(Node,true),
      loop(maps:put(Node,Pid,State));
    % Someone has turned the LEDs on
    {Node,{led,on}} ->
      case maps:get(Node,State,undefined) of
        % If the sender has not registered, ignore the message
        undefined -> loop(State);
        Pid ->
          io:fwrite("~p turned the leds on~n",[Pid]),
          % Send a message to every registered board telling it to turn its LEDs on
          Msg = {led,on},
          [P!Msg||P<-maps:values(State)],%,P =/= Pid],
          loop(State)
      end;
    % Someone has turned the LEDs off
    {Node,{led,off}} ->
      case maps:get(Node,State,undefined) of
        % If the sender has not registered, ignore the message
        undefined -> loop(State);
        Pid ->
          io:fwrite("~p turned the leds off~n",[Pid]),
          % Send a message to every registered board telling it to turn its LEDs off
          Msg = {led,off},
          [P!Msg||P<-maps:values(State)],%,P =/= Pid],
          loop(State)
      end;
    % Another message of unknown form
    Msg ->
      % Print it to the console
      io:fwrite("~p~n",[Msg]),
      loop(State)
  end.
