-module(bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Config) ->
    supervisor:start_child(?MODULE, [Config]).


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(Config) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 3600},
    {ok, {SupFlags, [bot_spec(Config)]}}.


%%====================================================================
%% Internal functions
%%====================================================================
bot_spec(Config) ->
    #{id => undefined,
      start => {bot, start_link, Config},
      restart => temporary,
      shutdown => 4000,
      type => worker,
      modules => [bot]}.
