-module(pong).
-behaviour(gen_server).

-export([handle_call/3,handle_cast/2,init/1]).

%-define(PADDLE_SIZE,10).

-record(vector,{x,y}).
-record(player,{pid=nil,pos=127,score=0}).
-record(ball,{pos=#vector{x=127,y=127},dir=#vector{x=0,y=0},last_touch=nil}).
-record(board,{ball=#ball{},players={nil,nil,nil,nil}}).

-type player() :: #player{} | nil.
-type players() :: {player(),player(),player(),player()}.


% The board appears as follows when processed:

%      C       
% 
% A          B
% 
%      D       

% It is modified before it is sent to players such that they appear on the left from their POV.


% TODO: Add functionality to add players at start
-spec get_new_board(players()) -> {ok, #board{}}.

get_new_board(_Players) ->
  Board = #board{},
  Ball = #ball{dir=normalise(#vector{x=math:random(),y=math:random()},10)},
  {ok, Board#board{ball=Ball}}.


-spec init(term()) -> {ok, #board{}}.

init(_Args) ->
  get_new_board(#{}).


-type cast() :: {add_player, pid()} | {remove_player, pid()} | tick | term().
-spec handle_cast({cast(),pid()},#board{}) -> {noreply,#board{}}.

handle_cast({add_player,Pid},Board) ->
  New_Players = add_player(Pid,Board#board.players),
  {noreply,Board#board{players=New_Players}};

handle_cast({remove_player,Pid},Board) ->
  New_Players = remove_player(Pid,Board#board.players),
  {noreply,Board#board{players=New_Players}};

handle_cast({change_pos,Pid,New_Pos},Board) ->
  New_Players = change_pos(Pid,New_Pos,Board#board.players),
  {noreply,Board#board{players=New_Players}};

handle_cast(tick,Board) ->
  case move_ball(Board) of
    {ok,New_Board} -> {noreply,New_Board};
    {ok,{goal,Pid,New_Score},New_Board} ->
      Pid!{new_score,New_Score},
      {noreply,New_Board}
    end;

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
change_pos(Pid,New_Pos,{A=#player{pid=Pid},B,C,D}) -> {A#player{pos=New_Pos},B,C,D};
change_pos(Pid,New_Pos,{A,B=#player{pid=Pid},C,D}) -> {A,B#player{pos=New_Pos},C,D};
change_pos(Pid,New_Pos,{A,B,C=#player{pid=Pid},D}) -> {A,B,C#player{pos=New_Pos},D};
change_pos(Pid,New_Pos,{A,B,C,D=#player{pid=Pid}}) -> {A,B,C,D#player{pos=New_Pos}};
change_pos(_,_,Board) -> Board.

-spec move_ball(#board{}) -> #board{}.

move_ball(Board=#board{players={_A,_B,_C,_D},ball=Ball}) ->
  #vector{x=Dx,y=Dy} = Ball#ball.dir,
  #vector{x=Px,y=Py} = Ball#ball.pos,
  case #vector{x=Px+Dx,y=Py+Dy} of
    % Bounce on top left corner or left goal
    #vector{x=X,y=Y} when X < 0 andalso Y < 0 andalso X < Y ->
      {ok,Board};
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when X < 0 andalso Y > 255 andalso X < 255-Y ->
      {ok,Board};
    % Bounce on left wall or left goal
    #vector{x=X} when X < 0 ->
      {ok,Board};

    % Bounce on top right corner or right goal
    #vector{x=X,y=Y} when X > 255 andalso Y < 0 andalso X > 255-Y ->
      {ok,Board};
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when X < 255 andalso Y > 255 andalso X > Y ->
      {ok,Board};
    % Bounce on right wall or right goal
    #vector{x=X} when X > 255 ->
      {ok,Board};

    % Bounce on top left corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X < 0 ->
      {ok,Board};
    % Bounce on top right corner or top goal
    #vector{x=X,y=Y} when Y < 0 andalso X > 255 ->
      {ok,Board};
    % Bounce on top wall or top goal
    #vector{y=Y} when Y < 0 ->
      {ok,Board};
    
    % Bounce on bottom left corner or left goal
    #vector{x=X,y=Y} when Y > 255 andalso X < 0 ->
      {ok,Board};
    % Bounce on bottom right corner or right goal
    #vector{x=X,y=Y} when Y > 255 andalso X > 255 ->
      {ok,Board};
    % Bounce on bottom wall or bottom goal
    #vector{y=Y} when Y > 255 ->
      {ok,Board};
    
    % No bounce
    New_Pos ->
      New_Ball = Ball#ball{pos=New_Pos},
      {ok,Board#board{ball=New_Ball}}
  end.


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

get_board(Pid,#board{players={A=#player{pid=Pid},B,C,D},ball=#ball{pos=Ball}}) ->
  Players = {A#player.pos,B#player.pos,C#player.pos,D#player.pos},
  Ball_Pos = {Ball#vector.x,Ball#vector.y},
  {ok,{Ball_Pos,Players}};

get_board(_,_) -> {error,not_player}.

