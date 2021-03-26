-module(pong_game).
-behaviour(gen_server).

-export([start_link/0,init/1]).
-export([handle_call/3,handle_cast/2]).

% Tweakable constants
-define(PADDLE_SIZE,25).
-define(BALL_SPEED,2).
-define(SPEED_INCREASE,0.125).
-define(SKEW_FACTOR,1.5).

% These are like structs
-record(vector,{x,y}).
-record(player,{pid=nil,pos=127,next_pos=127,score=0}).
-record(ball,{pos=#vector{x=127.5,y=127.5},dir=#vector{x=0,y=0},last_touch=nil,speed=?BALL_SPEED}).
-record(board,{ball=#ball{},players={nil,nil,nil,nil}}).

-type player() :: #player{} | nil.
-type players() :: {player(),player(),player(),player()}.

-spec start_link() -> gen_server:result().

% Start the game server & return the process id (PID) to the supervisor
start_link() ->
  gen_server:start_link({local,pong_game},pong_game,[],[]).


-spec init(term()) -> {ok, #board{}}.
% Start the game by creating a new board
init(_Args) ->
  io:fwrite("Starting pong game~n"),
  get_new_board(#{}).


% TODO: Add functionality to add players at start
-spec get_new_board(players()) -> {ok, #board{}}.

% Get a new board by returning a board with everything default
get_new_board(_Players) ->
  Board = #board{},
  Ball = #ball{},
  {ok, Board#board{ball=Ball}}.


-type cast() :: {add_player, pid()} | {remove_player, pid()} | tick | term().
-spec handle_cast({cast(),pid()},#board{}) -> {noreply,#board{}}.

% When an asynchronous request (a cast) to add a player is received
handle_cast({add_player,Pid},Board) ->
  % Add the player to the list of players
  New_Players = add_player(Pid,Board#board.players),
  io:fwrite("Player ~p added~n",[Pid]),
  % Don't send a response & update the board
  {noreply,Board#board{players=New_Players}};

% When an asynchronous request (a cast) to remove a player is received
handle_cast({remove_player,Pid},Board) ->
  % Remove the player from the list of players
  New_Players = remove_player(Pid,Board#board.players),
  io:fwrite("Player ~p left~n",[Pid]),
  % Don't send a response & update the board
  {noreply,Board#board{players=New_Players}};

% When an asynchronous request (a cast) to advance the server by 1 tick is received
handle_cast(tick,Board) ->
  {A,B,C,D} = Board#board.players,
  % If A is not playing, don't change them, otherwise update their position
  New_A = if A =:= nil -> A;
             true -> A#player{pos=A#player.next_pos} end,
  % If B is not playing, don't change them, otherwise update their position
  New_B = if B =:= nil -> B;
             true -> B#player{pos=B#player.next_pos} end,
  % If C is not playing, don't change them, otherwise update their position
  New_C = if C =:= nil -> C;
             true -> C#player{pos=C#player.next_pos} end,
  % If A is not playing, don't change them, otherwise update their position
  New_D = if D =:= nil -> D;
             true -> D#player{pos=D#player.next_pos} end,

  % Create a new list of players to update the new positions
  New_Players = {New_A,New_B,New_C,New_D},

  % See what the outcome of moving the ball is
  case move_ball(Board#board{players=New_Players}) of
    % If there's no goal, don't reply and set the state as the board with updated positions
    {ok,New_Board} -> {noreply,New_Board};
    % If there's a goal
    {ok,{goal,Pid,New_Score},New_Board} ->
      io:fwrite("~p new score: ~p~n",[Pid,New_Score]),
      % Send the player who scored their new score
      % `lhs ! rhs` means send a message containing the rhs to the lhs
      Pid!{new_score,New_Score},
      % Send all players the new speed of the ball
      [Player!{speed,?BALL_SPEED} || Player <- tuple_to_list(New_Players), Player /= nil],
      % Don't reply and set the state as the board with updated positions
      {noreply,New_Board}
  end;

% When an asynchronous request (a cast) to change position of a player is received
handle_cast({change_pos,Pid,New_Pos},Board) ->
  % Call the function to change the position of the player
  New_Players = change_pos(Pid,New_Pos,Board#board.players),
  % Don't reply & set the state to the board with updated player positions
  {noreply,Board#board{players=New_Players}};
  
% Ignore any other asynchronous request
handle_cast(_,Board) -> {noreply,Board}.


-spec add_player(pid(),players()) -> players().

% Add a player by finding the 1st slot which is free & assigning it to the new player
add_player(Pid,{nil,B,C,D}) -> {#player{pid=Pid},B,C,D};
add_player(Pid,{A,nil,C,D}) -> {A,#player{pid=Pid},C,D};
add_player(Pid,{A,B,nil,D}) -> {A,B,#player{pid=Pid},D};
add_player(Pid,{A,B,C,nil}) -> {A,B,C,#player{pid=Pid}};
% If the game is full, send a message to the player who attempted to join
add_player(Pid,Board) ->
  Pid!<<"The game is full, sorry">>,
  Board.


-spec remove_player(pid(),players()) -> players().

% Remove a player by looking for their ID and changing it to 'nil'
remove_player(Pid,{#player{pid=Pid},B,C,D}) -> {nil,B,C,D};
remove_player(Pid,{A,#player{pid=Pid},C,D}) -> {A,nil,C,D};
remove_player(Pid,{A,B,#player{pid=Pid},D}) -> {A,B,nil,D};
remove_player(Pid,{A,B,C,#player{pid=Pid}}) -> {A,B,C,nil};
% If the message came from a process who isn't a player anyway, ignore it
remove_player(_,Players) -> Players.


-spec change_pos(pid(),integer(),players()) -> players().

% When a player tries to move out of the board range, send them a message saying that
change_pos(Pid,New_Pos,Players) when (New_Pos > 255) or (New_Pos < 0) ->
  Msg = io_lib:format("~B is not within 0 and 255", [New_Pos]),
  Pid!list_to_binary(Msg),
  Players;
% Set the player's next position to move to
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

% When there are no players, reset the ball to the middle and set it not to move
move_ball(Board=#board{players={nil,nil,nil,nil}}) ->
  {ok,Board#board{ball=#ball{dir=#vector{x=0,y=0}}}};

% Move the ball normally
move_ball(Board=#board{players={A,B,C,D},ball=In_Ball}) ->
  % If the ball is stationary, make a new ball
  Ball = case In_Ball#ball.dir of
    #vector{x=0,y=0} -> new_ball();
    _ -> In_Ball
  end,
  % Get the direction & position of the ball
  #vector{x=Dx,y=Dy} = Ball#ball.dir,
  #vector{x=Px,y=Py} = Ball#ball.pos,
  % Check if the ball is going to go out of bounds & if so, call the bounce function
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
  % Update the scores following the bounce function (in case there was a goal)
  case Bounce of
    {Nb,nil} ->
      {New_Ball, New_Players} = {Nb,{A,B,C,D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,New_Board};
    {Nb,A} ->
      {New_Ball, New_Players} = {Nb,{A#player{score=A#player.score+1},B,C,D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,{goal,A#player.pid,A#player.score+1},New_Board};
    {Nb,B} ->
      {New_Ball, New_Players} = {Nb,{A,B#player{score=B#player.score+1},C,D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,{goal,B#player.pid,B#player.score+1},New_Board};
    {Nb,C} ->
      {New_Ball, New_Players} = {Nb,{A,B,C#player{score=C#player.score+1},D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,{goal,C#player.pid,C#player.score+1},New_Board};
    {Nb,D} ->
      {New_Ball, New_Players} = {Nb,{A,B,C,D#player{score=D#player.score+1}}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,{goal,D#player.pid,D#player.score+1},New_Board};
    {Nb,_} ->
      {New_Ball, New_Players} = {Nb,{A,B,C,D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,New_Board};
    Nb ->
      {New_Ball, New_Players} = {Nb,{A,B,C,D}},
      New_Board = Board#board{ball=New_Ball,players=New_Players},
      {ok,New_Board}
  end.


-type wall() :: left | top | right | bottom | top_left | top_right | bottom_right | bottom_left.
-type vector() :: {integer(),integer()}.
-spec bounce(wall(),#ball{},vector(),vector(),#player{}) -> {#ball{},#player{}} | #ball{}.

% When there is no player, bounce the ball off the wall without checking for collisions
bounce(left,Ball,_,_,nil) -> bounce_wall(x,Ball);
bounce(right,Ball,_,_,nil) -> bounce_wall(x,Ball);
bounce(bottom,Ball,_,_,nil) -> bounce_wall(y,Ball);
bounce(top,Ball,_,_,nil) -> bounce_wall(y,Ball);
bounce(_,Ball,_,_,nil) -> bounce_wall(xy,Ball);

% When there is a player, check for collisions
bounce(left,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("left~n"),
  case detect_collision(X1,Y1,X2,Y2,0,Player) of
    % No collision means a goal, so we return a new ball & the last player to touch the ball
    {_,false} ->
      io:fwrite("Last: ~p~n",[Ball#ball.last_touch]),
      {new_ball(),Ball#ball.last_touch};
    % This is for error checking, if somehow the player is invalid bounce the ball off the wall
    {0, _} -> bounce_wall(x,Ball);
    % Call the bounce paddle function if there is a collision `Diff` units from the centre of the paddle
    {Diff, _} -> (bounce_paddle(x,Diff,Ball))#ball{last_touch = Player}
  end;

% These are all the same just rotated etc. for different walls
bounce(right,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("right~n"),
  case detect_collision(X1,Y1,X2,Y2,255,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} -> bounce_wall(x,Ball);
    {Diff, _} -> (bounce_paddle(x,Diff,Ball))#ball{last_touch = Player}
  end;

bounce(top,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("top~n"),
  case detect_collision(Y1,X1,Y2,X2,255,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} ->
      io:fwrite("Wall~n"),
      bounce_wall(y,Ball);
    {Diff, _} -> (bounce_paddle(x,Diff,Ball))#ball{last_touch = Player}
  end;

bounce(bottom,Ball,{X1,Y1},{X2,Y2},Player) ->
  io:fwrite("bottom~n"),
  case detect_collision(Y1,X1,Y2,X2,0,Player) of
    {_,false} -> {new_ball(),Ball#ball.last_touch};
    {0, _} -> bounce_wall(y,Ball);
    {Diff, _} -> (bounce_paddle(x,Diff,Ball))#ball{last_touch = Player}
  end;

% For a corner bounce, just return the ball in the opposite direction it came from because corner bounces are scary
% I could implement it but atm I'm just handwaving it
bounce(_,Ball,_,_,_) ->
  io:fwrite("corner~n"),
  % TODO: Corner bounce detection
  bounce_wall(xy, Ball).


-spec detect_collision(integer(),integer(),integer(),integer(),integer(),#player{}) -> boolean().

% When there is no player, there is a collision with the wall so return true with no skew.
detect_collision(_,_,_,_,_,nil) ->
  {0, true};

detect_collision(X1,Y1,X2,Y2,Wall,Player) ->
  io:fwrite("(~p,~p) => (~p,~p)~n",[X1,Y1,X2,Y2]),
  io:fwrite("Player: ~p~n",[Player]),
  % y = Mx + c :) 
  Dy = Y2-Y1,
  Dx = X2-X1,
  M = Dy/Dx,
  C = Y1 - M * X1,
  B_Pos = M * Wall + C,
  P_Pos = Player#player.pos,
  io:fwrite("~p, ~p~n",[B_Pos,P_Pos]),
  % Check if the position of the ball as it crosses the goal-line intersects the paddle
  Is_Hit = P_Pos + ?PADDLE_SIZE/2 >= B_Pos andalso P_Pos - ?PADDLE_SIZE/2 =< B_Pos,
  % Return the distance from the centre of the paddle & whether it hit the paddle
  {B_Pos - P_Pos, Is_Hit}.


-type direction() :: x | y | xy.
-spec bounce_wall(direction(),#ball{}) -> #ball{}.

% Bounce off the walls by increasing the speed
% and negating the speed vector in the appropriate axes
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

% Bounce off a paddle in a similar way
% however rather than the speed vector simply being negated
% it is instead negated & a skew added to the value which is negated
bounce_paddle(x,Diff,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Skew = Diff / ?PADDLE_SIZE * ?SKEW_FACTOR * Ball#ball.speed,
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=-X,y=Y+Skew},N_Speed),speed=N_Speed};
bounce_paddle(y,Diff,Ball=#ball{dir=#vector{x=X,y=Y}}) ->
  Skew = Diff / ?PADDLE_SIZE * ?SKEW_FACTOR * Ball#ball.speed,
  io:fwrite("Speed: ~p~n",[Ball#ball.speed]),
  N_Speed = Ball#ball.speed+?SPEED_INCREASE,
  Ball#ball{dir=normalise(#vector{x=X+Skew,y=-Y},N_Speed),speed=N_Speed};
bounce_paddle(xy,_,Ball) ->
  bounce_wall(xy,Ball).

-spec normalise(#vector{}, number()) -> #vector{}.
% Normalise the vector to a constant speed by taking the hypotenuse of the vector
% to find the appropriate scaling value (idk if it's right but it seems to work)
normalise(#vector{x=X,y=Y}, Speed) ->
  N = math:sqrt(X*X+Y*Y) / Speed,
  #vector{x=X/N,y=Y/N}.


-type call() :: {get_board, pid()} | get_speed | term().
-type call_reply() :: {error,term()} | {ok,term()}.
-spec handle_call(call(),term(),#board{}) -> {reply,call_reply(),#board{}}.

% Handle a synchronous request to get the current board by responding with the current board
% from the pov of the player whom requested it
handle_call({get_board,Pid},_From,Board) ->
  {reply,get_board(Pid,Board),Board};

handle_call(get_speed, _From, Board=#board{ball=#ball{speed=Speed}}) ->
  {reply,Speed,Board};

% Respond to any other (unsolicited!) message with an error
handle_call(Msg,_From,Board) ->
  {reply,{error,{unknown,Msg}},Board}.


-type ball_pos() :: {number(),number()}.
-type player_pos() :: {number(),number(),number(),number()}.
-type board_state() :: {ball_pos(),player_pos()}.
-spec get_board(pid(),#board{}) -> {ok,board_state()} | {error,atom()}.

% When player A requested the board, send the positions of each player & the ball
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

% If players B-D request the ball
% rotate the game state accordingly such that they appear as player A and on the left
get_board(Pid,Board=#board{players={A,B=#player{pid=Pid},C,D}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=255-Pos#vector.x, y=Pos#vector.y},
  Flipped_Board = Board#board{players={B,A,C,D},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board=#board{players={A,B,C=#player{pid=Pid},D}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=Pos#vector.y, y=255-Pos#vector.x},
  Flipped_Board = Board#board{players={C,D,B,A},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board=#board{players={A,B,C,D=#player{pid=Pid}}, ball=Ball=#ball{pos=Pos}}) ->
  New_Ball_Pos = Pos#vector{x=255-Pos#vector.y, y=255-Pos#vector.x},
  Flipped_Board = Board#board{players={D,C,A,B},ball=Ball#ball{pos=New_Ball_Pos}},
  get_board(Pid,Flipped_Board);

get_board(Pid,Board) ->
  io:fwrite("~p~n~p~n",[Pid,Board]),
  {error,not_board}.


-spec new_ball() -> #ball{}.

% Get a new ball by setting the position to the middle of the screen
% and generating a random vector between -0.5 and 0.5 & normalising it
new_ball() ->
  Pos = #vector{x=127.5,y=127.5},
  Dir = normalise(#vector{x=rand:uniform()-0.5,y=rand:uniform()-0.5},?BALL_SPEED),
  #ball{pos=Pos,dir=Dir}.