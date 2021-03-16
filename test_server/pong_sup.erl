-module(pong_sup).
-behaviour(supervisor).

-export([start/0,init/1]).

start() ->
  supervisor:start_link(pong_sup,[]).


-spec init(term()) -> {ok,term(),term()}.

init(_Args) ->
  Sup_Flags = #{strategy => one_for_one, intensity => 1, period => 5},
  Pong_App = #{id => pong_server,
               start => {pong_server,start_link,[]},
               restart => permanent,
               shutdown => brutal_kill,
               type => worker,
               modules => [pong_server]},
  Pong_Game = #{id => pong_game,
                start => {pong_game,start_link,[]},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => [pong_game]},
  {ok,{Sup_Flags,[Pong_App,Pong_Game]}}.