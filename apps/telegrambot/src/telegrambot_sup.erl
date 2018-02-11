%%
%% Copyright Peter Dimitrov 2018, All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

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
init([_Config]) ->
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
