-module(pong_game).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).

-define(PADDLE_SIZE,25).
-define(BALL_SPEED,2).
-define(SPEED_INCREASE,0.125).
-define(SKEW_FACTOR,1.5).

-record(vector,{x,y}).
-record(player,{pid=nil,pos=127,next_pos=127,score=0}).
-record(ball,{pos=#vector{x=127.5,y=127.5},dir=#vector{x=0,y=0},last_touch=nil,speed=?BALL_SPEED}).
-record(board,{ball=#ball{},players={nil,nil,nil,nil}}).

-type player() :: #player{} | nil.
-type players() :: {player(),player(),player(),player()}.

-spec start_link() -> gen_server:result().

start_link() ->
  gen_server:start_link({local,pong_game},pong_game,[],[]).


-spec init(term()) -> {ok, #board{}}.

init(_Args) ->
  io:fwrite("Starting pong game~n"),
  get_new_board(#{}).


% TODO: Add functionality to add players at start
-spec get_new_board(players()) -> {ok, #board{}}.

get_new_board(_Players) ->
  Board = #board{},
  Ball = #ball{},
  {ok, Board#board{ball=Ball}}.


-type cast() :: {add_player, pid()} | {remove_player, pid()} | tick | term().
-spec handle_cast({cast(),pid()},#board{}) -> {noreply,#board{}}.

handle_cast({add_player,Pid},Board) ->
  New_Players = add_player(Pid,Board#board.players),
  io:fwrite("Player ~p added~n",[Pid]),
  {noreply,Board#board{players=New_Players}};

handle_cast({remove_player,Pid},Board) ->
  New_Players = remove_player(Pid,Board#board.players),
  io:fwrite("Player ~p left~n",[Pid]),
  {noreply,Board#board{players=New_Players}};

handle_cast(tick,Board) ->
  {A,B,C,D} = Board#board.players,
  New_A = if A =:= nil -> A;
             true -> A#player{pos=A#player.next_pos} end,
  New_B = if B =:= nil -> B;
             true -> B#player{pos=B#player.next_pos} end,
  New_C = if C =:= nil -> C;
             true -> C#player{pos=C#player.next_pos} end,
  New_D = if D =:= nil -> D;
             true -> D#player{pos=D#player.next_pos} end,
  New_Players = {New_A,New_B,New_C,New_D},
  case move_ball(Board#board{players=New_Players}) of
    {ok,New_Board} -> {noreply,New_Board};
    {ok,{goal,Pid,New_Score},New_Board} ->
      Pid!{new_score,New_Score},
      {noreply,New_Board}
    end;

handle_cast({change_pos,Pid,New_Pos},Board) ->
  New_Players = change_pos(Pid,New_Pos,Board#board.players),
  {noreply,Board#board{players=New_Players}};
  

handle_cast(_,Board) -> {noreply,Board}.


-spec add_player(pid(),players()) -> players().

add_player(Pid,{nil,B,C,D}) -> {#player{pid=Pid},B,C,D};
add_player(Pid,{A,nil,C,D}) -> {A,#player{pid=Pid},C,D};
add_player(Pid,{A,B,nil,D}) -> {A,B,#player{pid=Pid},D};
add_player(Pid,{A,B,C,nil}) -> {A,B,C,#player{pid=Pid}};
add_player(Pid,Board) ->
  Pid!<<"The game is full, sorry">>,
  Board.


-spec remove_player(pid(),players()) -> players().

remove_player(Pid,{#player{pid=Pid},B,C,D}) -> {nil,B,C,D};
remove_player(Pid,{A,#player{pid=Pid},C,D}) -> {A,nil,C,D};
remove_player(Pid,{A,B,#player{pid=Pid},D}) -> {A,B,nil,D};
remove_player(Pid,{A,B,C,#player{pid=Pid}}) -> {A,B,C,nil};
remove_player(_,Players) -> Players.


-spec change_pos(pid(),integer(),players()) -> players().

change_pos(Pid,New_Pos,Players) when (New_Pos > 255) or (New_Pos < 0) ->
  Msg = io_lib:format("~B is not within 0 and 255", [New_Pos]),
  Pid!list_to_binary(Msg),
  Players;
change_pos(Pid,New_Pos,{A=#player{pid=Pid},B,C,D}) ->
  io:fwrite("Changing A to ~B~n",[New_Pos]),
  {A#player{next_pos=New_Pos},B,C,D};
change_pos(Pid,New_Pos,{A,B=#player{pid=Pid},C,D}) ->
  io:fwrite("Changing B to ~B~n",[New_Pos]),
  {A,B#player{next_pos=New_Pos},C,D};
change_pos(Pid,New_Pos,{A,B,C=#player{pid=Pid},D}) ->
  io:fwrite("Changing C to ~B~n",[New_Pos]),
  {A,B,C#player{next_pos=New_Pos},D};
change_pos(Pid,New_Pos,{A,B,C,D=#player{pid=Pid}}) ->
  io:fwrite("Changing D to ~B~n",[New_Pos]),
  {A,B,C,D#player{next_pos=New_Pos}};
change_pos(_,_,Board) -> Board.

-spec move_ball(#board{}) -> #board{}.

% When there are no players, reset the ball
move_ball(Board=#board{players={nil,nil,nil,nil}}) ->
  {ok,Board#board{ball=#ball{dir=#vector{x=0,y=0}}}};

move_ball(Board=#board{players={A,B,C,D},ball=In_Ball}) ->
  Ball = case In_Ball#ball.dir of
    #vector{x=0,y=0} -> new_ball();
    _ -> In_Ball
  end,
  #vector{x=Dx,y=Dy} = Ball#ball.dir,
  #vector{x=Px,y=Py} = Ball#ball.pos,
  Bounce = case #vector{x=Px+Dx,y=Py+Dy} of
    % Bounce on top left corner or left goal
    #vector{x=X,y=Y} when X < 1 andalso Y < 1 andalso X < Y ->
      bounce(top_left,Ball,{Px,Py},{X,Y},A);
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when X < 1 andalso Y > 254 andalso X < 254-Y ->
      bounce(bottom_left,Ball,{Px,Py},{X,Y},A);
    % Bounce on left wall or left goal
    #vector{x=X,y=Y} when X < 1 ->
      bounce(left,Ball,{Px,Py},{X,Y},A);

    % Bounce on top right corner or right goal
    #vector{x=X,y=Y} when X > 254 andalso Y < 1 andalso X > 254-Y ->
      bounce(top_right,Ball,{Px,Py},{X,Y},B);
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when X < 254 andalso Y > 255 andalso X > Y ->
      bounce(bottom_right,Ball,{Px,Py},{X,Y},B);
    % Bounce on right wall or right goal
    #vector{x=X,y=Y} when X > 255 ->
      bounce(right,Ball,{Px,Py},{X,Y},B);

    % Bounce on top left corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X < 0 ->
      bounce(top_left,Ball,{Px,Py},{X,Y},C);
    % Bounce on top right corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X > 255 ->
      bounce(top_right,Ball,{Px,Py},{X,Y},C);
    % Bounce on top wall or top goal
    #vector{x=X,y=Y} when Y < 0 ->
      bounce(top,Ball,{Px,Py},{X,Y},C);
    
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when Y > 255 andalso X < 0 ->
      bounce(bottom_left,Ball,{Px,Py},{X,Y},D);
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when Y > 255 andalso X > 255 ->
      bounce(bottom_right,Ball,{Px,Py},{X,Y},D);
    % Bounce on bottom wall or bottom goal
    #vector{x=X,y=Y} when Y > 255 ->
      bounce(bottom,Ball,{Px,Py},{X,Y},D);
    
    % No bounce
    New_Pos ->
      Ball#ball{pos=New_Pos}
  end,
  {New_Ball, New_Players} = case Bounce of
    {Nb,a} -> {Nb,{A#player{score=A#player.score+1},B,C,D}};
    {Nb,b} -> {Nb,{A,B#player{score=B#player.score+1},C,D}};
    {Nb,c} -> {Nb,{A,B,C#player{score=C#player.score+1},D}};
    {Nb,d} -> {Nb,{A,B,C,D#player{score=D#player.score+1}}};
    {Nb,_} -> {Nb,{A,B,C,D}};
    Nb -> {Nb,{A,B,C,D}}
  end,
  New_Board = Board#board{ball=New_Ball,players=New_Players},
  {ok,New_Board}.


-type wall() :: left | top | right | bottom | top_left | top_right | bottom_right | bottom_left.
-type vector() :: {integer(),integer()}.
-spec bounce(wall(),#ball{},vector(),vector(),#player{}) -> {#ball{},#player{}} | #ball{}.


bounce(left,Ball,_,_,nil) -> bounce_wall(x,Ball);
bounce(right,Ball,_,_,nil) -> bounce_wall(x,Ball);
bounce(bottom,Ball,_,_,nil) -> bounce_wall(y,Ball);
bounce(top,Ball,_,_,nil) -> bounce_wall(y,Ball);
bounce(_,Ball,_,_,nil) -> bounce_wall(xy,Ball);

bounce(left,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("left~n"),
  case detect_collision(X1,Y1,X2,Y2,0,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} -> bounce_wall(x,Ball);
    {Diff, _} -> bounce_paddle(x,Diff,Ball)
  end;

bounce(right,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("right~n"),
  case detect_collision(X1,Y1,X2,Y2,255,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} -> bounce_wall(x,Ball);
    {Diff, _} -> bounce_paddle(x,Diff,Ball)
  end;

bounce(top,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("top~n"),
  case detect_collision(Y1,X1,Y2,X2,0,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} ->
      io:fwrite("Wall~n"),
      bounce_wall(y,Ball);
    {Diff, _} ->
      io:fwrite("Paddle: ~p~n",[Diff]),
      bounce_paddle(y,Diff,Ball)
  end;

bounce(bottom,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("bottom~n"),
  case detect_collision(Y1,X1,Y2,X2,255,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} -> bounce_wall(y,Ball);
    {Diff, _} -> bounce_paddle(y,Diff,Ball)
  end;

bounce(_,Ball,_,_,_) ->
  io:fwrite("corner~n"),
  % TODO: Corner bounce detection
  bounce_wall(xy, Ball).


-spec detect_collision(integer(),integer(),integer(),integer(),integer(),#player{}) -> boolean().

detect_collision(_,_,_,_,_,nil) ->
  {0, true};

detect_collision(X1,Y1,X2,Y2,Wall,Player) ->
  io:fwrite("(~p,~p) => (~p,~p)~n",[X1,Y1,X2,Y2]),
  io:fwrite("Player: ~p~n",[Player]),
  Dy = Y2-Y1,
  Dx = X2-X1,
  M = Dy/Dx,
  C = Y1 - M * X1,
  B_Pos = M * Wall + C,
  P_Pos = Player#player.pos,
  io:fwrite("~p, ~p~n",[B_Pos,P_Pos]),
  Is_Hit = P_Pos + ?PADDLE_SIZE/2 >= B_Pos andalso P_Pos - ?PADDLE_SIZE/2 =< B_Pos,
  {B_Pos - P_Pos, Is_Hit}.


-type direction() :: x | y | xy.
-spec bounce_wall(direction(),#ball{}) -> #ball{}.

bounce_wall(x,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=-X,y=Y},N_Speed),speed=N_Speed};
bounce_wall(y,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=X,y=-Y},N_Speed),speed=N_Speed};
bounce_wall(xy,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=-X,y=-Y},N_Speed),speed=N_Speed}.


-spec bounce_paddle(direction(),integer(),#ball{}) -> #ball{}.

bounce_paddle(x,Diff,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  %Skew = math:pow(?SKEW_FACTOR,-Diff),
  Skew = Diff / ?PADDLE_SIZE * ?SKEW_FACTOR * Ball#ball.speed,
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=-X-Skew,y=Y},N_Speed),speed=N_Speed};
bounce_paddle(y,Diff,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Skew = 1 - abs(Diff / ?PADDLE_SIZE),
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=X,y=-Y-Skew},N_Speed),speed=N_Speed};
bounce_paddle(xy,_,Ball) ->
  bounce_wall(xy,Ball).

-spec normalise(#vector{}, number()) -> #vector{}.

normalise(#vector{x=X,y=Y}, Speed) ->
  N = math:sqrt(X*X+Y*Y) / Speed,
  #vector{x=X/N,y=Y/N}.


-type call() :: {get_board, pid()} | term().
-type call_reply() :: {error,term()} | {ok,term()}.
-spec handle_call(call(),term(),#board{}) -> {reply,call_reply(),#board{}}.

handle_call({get_board,Pid},_From,Board) ->
  {reply,get_board(Pid,Board),Board};

handle_call(Msg,_From,Board) ->
  {reply,{error,{unknown,Msg}},Board}.


-type ball_pos() :: {number(),number()}.
-type player_pos() :: {number(),number(),number(),number()}.
-type board_state() :: {ball_pos(),player_pos()}.
-spec get_board(pid(),#board{}) -> {ok,board_state()} | {error,atom()}.

get_board(Pid,#board{players={A=#player{pid=Pid},B,C,D}, ball=#ball{pos=Ball}}) ->
  A_Pos = A#player.pos,
  B_Pos = if B =:= nil -> nil;
          true -> B#player.pos end,
  C_Pos = if C =:= nil -> nil;
          true -> C#player.pos end,
  D_Pos = if D =:= nil -> nil;
          true -> D#player.pos end,
  Players = {A_Pos,B_Pos,C_Pos,D_Pos},
  Ball_Pos = {Ball#vector.y,Ball#vector.x},
  {ok,{Ball_Pos,Players}};

get_board(Pid,Board=#board{players={A,B=#player{pid=Pid},C,D}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=255-Pos#vector.x, y=255-Pos#vector.y},
  Flipped_Board = Board#board{players={B,A,D,C},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board=#board{players={A,B,C=#player{pid=Pid},D}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=Pos#vector.y, y=255-Pos#vector.x},
  Flipped_Board = Board#board{players={C,D,B,A},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board=#board{players={A,B,C,D=#player{pid=Pid}}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=255-Pos#vector.y, y=Pos#vector.x},
  Flipped_Board = Board#board{players={D,C,A,B},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board) ->
  io:fwrite("~p~n~p~n",[Pid,Board]),
  {error,not_board}.


-spec new_ball() -> #ball{}.

new_ball() ->
  Pos = #vector{x=127.5,y=127.5},
  Dir = normalise(#vector{x=rand:uniform()-0.5,y=rand:uniform()-0.5},?BALL_SPEED),
  #ball{pos=Pos,dir=Dir}.