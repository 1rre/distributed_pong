-module(pong_server).

-export([start_link/0,init/0]).

-spec start_link() -> gen_event:result().

start_link() ->
  {ok,erlang:spawn_link(?MODULE,init,[])}.


-spec init() -> nil().

init() ->
  io:fwrite("Starting pong server~n"),
  register(pong_server,self()),
  Ref = make_ref(),
  timer:send_interval(100,self(),{Ref,tick}),
  loop(#{},Ref).


-spec loop(map(),reference()) -> nil().

loop(Nodes,Ref) -> receive
  {register,Pid,Node} ->
    monitor_node(Node,true),
    gen_server:cast(pong_game,{add_player,Pid}),
    loop(maps:put(Node,Pid,Nodes),Ref);
  {nodedown,Node} ->
    case maps:get(Node,Nodes,nil) of
      nil -> ok;
      Pid -> gen_server:cast(pong_game,{remove_player,Pid})
    end,
    loop(maps:remove(Node,Nodes),Ref);
  {Ref,tick} ->
    [Pid!gen_server:call(pong_game,{get_board,Pid}) || Pid <- maps:values(Nodes), pid /= nil],
    gen_server:cast(pong_game,tick),
    loop(Nodes,Ref);
  {change_pos,Pid,Pos} ->
    gen_server:cast(pong_game,{change_pos,Pid,Pos}),
    loop(Nodes,Ref)
end.
