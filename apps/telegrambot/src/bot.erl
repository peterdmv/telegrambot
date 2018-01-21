-module(bot).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(ChatId) ->
    BotName = bot_name(ChatId),
    Server = {local, BotName},
    Opts = [],
    gen_server:start_link(Server, ?MODULE, ChatId, Opts).


%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init(_ChatId) ->
    {ok, []}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

bot_name(ChatId) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(ChatId)).
