-module(pong_game).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).

%-define(PADDLE_SIZE,10).
-define(BALL_SPEED,10).

-record(vector,{x,y}).
-record(player,{pid=nil,pos=127,next_pos=127,score=0}).
-record(ball,{pos=#vector{x=127,y=127},
              dir=#vector{x=0,y=0},
              last_touch=nil}).
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
  case move_ball(Board) of
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
change_pos(Pid,New_Pos,{A=#player{pid=Pid},B,C,D}) -> {A#player{next_pos=New_Pos},B,C,D};
change_pos(Pid,New_Pos,{A,B=#player{pid=Pid},C,D}) -> {A,B#player{next_pos=New_Pos},C,D};
change_pos(Pid,New_Pos,{A,B,C=#player{pid=Pid},D}) -> {A,B,C#player{next_pos=New_Pos},D};
change_pos(Pid,New_Pos,{A,B,C,D=#player{pid=Pid}}) -> {A,B,C,D#player{next_pos=New_Pos}};
change_pos(_,_,Board) -> Board.

-spec move_ball(#board{}) -> #board{}.

% When there are no players, reset the ball
move_ball(Board=#board{players={nil,nil,nil,nil}}) ->
  {ok,Board#board{ball=#ball{dir=#vector{x=0,y=0}}}};

move_ball(Board=#board{players={_A,_B,_C,_D},ball=In_Ball}) ->
  Ball = case In_Ball#ball.dir of
    #vector{x=0,y=0} ->
      In_Ball#ball{dir=normalise(#vector{x=rand:uniform()-0.5,y=rand:uniform()-0.5},?BALL_SPEED)};
    _ -> In_Ball
  end,
  #vector{x=Dx,y=Dy} = Ball#ball.dir,
  #vector{x=Px,y=Py} = Ball#ball.pos,
  case #vector{x=Px+Dx,y=Py+Dy} of
    % Bounce on top left corner or left goal
    #vector{x=X,y=Y} when X < 0 andalso Y < 0 andalso X < Y ->
      New_Board = Board#board{ball=bounce_wall(xy,Ball)},
      {ok,New_Board};
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when X < 0 andalso Y > 255 andalso X < 255-Y ->
      New_Board = Board#board{ball=bounce_wall(xy,Ball)},
      {ok,New_Board};
    % Bounce on left wall or left goal
    #vector{x=X} when X < 0 ->
      New_Board = Board#board{ball=bounce_wall(x,Ball)},
      {ok,New_Board};

    % Bounce on top right corner or right goal
    #vector{x=X,y=Y} when X > 255 andalso Y < 0 andalso X > 255-Y ->
      New_Board = Board#board{ball=bounce(top_right,right,Ball)},
      {ok,New_Board};
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when X < 255 andalso Y > 255 andalso X > Y ->
      New_Board = Board#board{ball=bounce(bottom_right,right,Ball)},
      {ok,New_Board};
    % Bounce on right wall or right goal
    #vector{x=X} when X > 255 ->
      New_Board = Board#board{ball=bounce(right,right,Ball)},
      {ok,New_Board};

    % Bounce on top left corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X < 0 ->
      New_Board = Board#board{ball=bounce(top_left,left,Ball)},
      {ok,New_Board};
    % Bounce on top right corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X > 255 ->
      New_Board = Board#board{ball=bounce(bottom_left,left,Ball)},
      {ok,New_Board};
    % Bounce on top wall or top goal
    #vector{y=Y} when Y < 0 ->
      New_Board = Board#board{ball=bounce(top,top,Ball)},
      {ok,New_Board};
    
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when Y > 255 andalso X < 0 ->
      New_Board = Board#board{ball=bounce(bottom_left,bottom,Ball)},
      {ok,New_Board};
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when Y > 255 andalso X > 255 ->
      New_Board = Board#board{ball=bounce(bottom_right,bottom,Ball)},
      {ok,New_Board};
    % Bounce on bottom wall or bottom goal
    #vector{y=Y} when Y > 255 ->
      New_Board = Board#board{ball=bounce(bottom,bottom,Ball)},
      {ok,New_Board};
    
    % No bounce
    New_Pos ->
      New_Ball = Ball#ball{pos=New_Pos},
      {ok,Board#board{ball=New_Ball}}
  end.


-type goal() :: left | top | right | bottom.
-type wall() :: goal() | top_left | top_right | bottom_right | bottom_left.
-spec bounce(wall(),goal(),#ball{}) -> #ball{}.

bounce(left,_Goal,Ball) ->
  % Paddle detection goes here
  bounce_wall(x,Ball);

bounce(right,_Goal,Ball) ->
  % Paddle detection goes here
  bounce_wall(x,Ball);

bounce(top,_Goal,Ball) ->
  % Paddle detection goes here
  bounce_wall(y,Ball);

bounce(bottom,_Goal,Ball) ->
  % Paddle detection goes here
  bounce_wall(y,Ball);

bounce(_Wall, _Goal, Ball) ->
  % Paddle detection goes here
  % Corners will be much trickier than just walls
  bounce_wall(xy,Ball).


-type direction() :: x | y | xy.
-spec bounce_wall(direction(),#ball{}) -> #ball{}.

bounce_wall(x,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Ball#ball{dir=#vector{x=-X,y=Y}};
bounce_wall(y,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Ball#ball{dir=#vector{x=X,y=-Y}};
bounce_wall(xy,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Ball#ball{dir=#vector{x=-X,y=-Y}}.

-spec normalise(#vector{}, number()) -> #vector{}.

normalise(#vector{x=X,y=Y}, Speed) ->
  N = math:sqrt(X*X+Y*Y) * Speed,
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
          true -> {C#player.pos,C#player.pos#vector.y} end,
  D_Pos = if D =:= nil -> nil;
          true -> {D#player.pos#vector.x,B#player.pos#vector.y} end,
  Players = {A_Pos,B_Pos,C_Pos,D_Pos},
  Ball_Pos = {Ball#vector.x,Ball#vector.y},
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
  Flipped_Board = Board#board{players={B,A,D,C},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board) ->
  io:fwrite("~p~n~p~n",[Pid,Board]),
  {error,not_board}.
