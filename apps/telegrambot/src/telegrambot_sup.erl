%%%-------------------------------------------------------------------
%% @doc telegrambot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(telegrambot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    init([[]]);
init([Config]) ->
   SupFlags = #{strategy => one_for_one,
		intensity => 10,
		period => 3600},
    {ok, {SupFlags, child_specs(self())}}.


%%====================================================================
%% Internal functions
%%====================================================================
child_specs(Config) ->
    [http_gateway(Config)].

http_gateway(Config) ->
    #{id => http_gateway,
      start => {http_gateway, start_link, [Config]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [http_gateway]}.

%% bot_sup() ->
%%     #{id => bot_sup,
%%       start => {bot_sup, start_link, []},
%%       restart => permanent,
%%       shutdown => infinity,
%%       type => supervisor,
%%       modules => [bot_sup]}.
