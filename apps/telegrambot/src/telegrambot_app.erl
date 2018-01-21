%%%-------------------------------------------------------------------
%% @doc telegrambot public API
%% @end
%%%-------------------------------------------------------------------

-module(telegrambot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    telegrambot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================