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
