-module(pong_sup).
-behaviour(supervisor).

-export([start/0,init/1]).

% When the supervisor is started, call the default method to start a supervisor
start() ->
  supervisor:start_link(pong_sup,[]).


-spec init(term()) -> {ok,term(),term()}.

% Set the processes/servers to be supervised & their configuration
init(_Args) ->
  % When *one* supervised process goes down,
  % only restart that *one* (strategy = one for one)
  % After more than *1* crash in *5* seconds,
  % give up on restarting the processes (intensity = 1 & period = 5)
  Sup_Flags = #{strategy => one_for_one, intensity => 1, period => 5},

  % The pong server's configuration
  % These are all pretty standard & frankly boring.
  % You don't need to know what they do.
  % I don't even know what they do.
  Pong_App = #{id => pong_server,
               start => {pong_server,start_link,[]},
               restart => permanent,
               shutdown => brutal_kill,
               type => worker,
               modules => [pong_server]},
  % The game's configuration
  % See above
  Pong_Game = #{id => pong_game,
                start => {pong_game,start_link,[]},
                restart => permanent,
                shutdown => brutal_kill,
                type => worker,
                modules => [pong_game]},
  % Return an 'ok' constant along with the supervisor flags & the processes to start
  {ok,{Sup_Flags,[Pong_App,Pong_Game]}}.