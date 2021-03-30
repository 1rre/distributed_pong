-module(pong_server).

-export([start_link/0,init/0]).
% Set the tick rate to 1 every 25ms
-define(TICK_RATE,25).


-spec start_link() -> {ok, pid()}.

% Start the server & return the process id (PID) so it can be monitored by the supervisor
start_link() ->
  {ok,erlang:spawn_link(?MODULE,init,[])}.


-spec init() -> nil().

% Start the server by registering it so it can be sent messages at a constant name
% and setting up a timer to send ticks to the server
init() ->
  io:fwrite("Starting pong server~n"),
  register(pong_server,self()),
  Ref = make_ref(),
  timer:send_interval(?TICK_RATE,self(),{Ref,tick}),
  loop(#{},Ref).


-spec loop(map(),reference()) -> nil().

% Receive messages on a loop
loop(Nodes,Ref) -> receive
  % A new player on node `Node` wants to register
  {register,Pid,Node} ->
    % Start monitoring the node to see when it goes down
    monitor_node(Node,true),
    % Add the player to the game
    gen_server:cast(pong_game,{add_player,Pid}),
    % Link the node to the PID so we know which player to remove if the node goes down
    % and loop back to receive more messages
    loop(maps:put(Node,Pid,Nodes),Ref);
  % A node has gone down
  {nodedown,Node} ->
    % Check if the node has been linked to the PID of a player
    case maps:get(Node,Nodes,nil) of
      % If not, do nothing
      nil -> ok;
      % If it is a player, remove them from the game
      Pid -> gen_server:cast(pong_game,{remove_player,Pid})
    end,
    % Remove the node from the linked nodes & loop to receive more messages
    loop(maps:remove(Node,Nodes),Ref);
  % If there is a tick with the unique reference of the timer (yes I distrust you guys enough to do this)
  {Ref,tick} ->
    % Synchronously send each player the board from their point of view
    % (to avoid it being affected by the tick which happens next & could result in different players getting different game states)
    [Pid!gen_server:call(pong_game,{get_board,Pid}) || Pid <- maps:values(Nodes), pid /= nil],
    gen_server:cast(pong_game,tick),
    Speed = gen_server:call(pong_game,get_speed),
    [Pid!{speed,Speed} || Pid <- maps:values(Nodes), pid /= nil],
    % Advance the game by 1 tick
    % Loop to receive more messages
    loop(Nodes,Ref);
  % If a player wants to change position
  {change_pos,Pid,Pos} ->
    % Asynchronously change their position
    gen_server:cast(pong_game,{change_pos,Pid,Pos}),
    % Loop to receive more messages
    loop(Nodes,Ref);
  % Any other message
  Msg ->
    % Print it to the console for debugging purposes
    io:fwrite("~p~n",[Msg]),
    % Loop to receive more messages
    loop(Nodes,Ref)
end.
