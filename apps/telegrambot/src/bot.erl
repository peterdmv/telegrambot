-module(bot).

-behaviour(gen_server).

%% API
-export([start_link/1, update/2]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {chatid}).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(ChatId) ->
    BotName = bot_name(ChatId),
    Server = {local, BotName},
    Opts = [],
    gen_server:start_link(Server, ?MODULE, ChatId, Opts).


update(Pid, Message) ->
    gen_server:cast(Pid, {update, Message}).

%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init(ChatId) ->
    {ok, #state{chatid=ChatId}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({update, Message}, State = #state{chatid=ChatId}) ->
    Text = maps:get(<<"text">>,maps:get(<<"message">>, Message)),
    %% Date = maps:get(<<"date">>,maps:get(<<"message">>, Message)),
    http_gateway:reply(integer_to_list(ChatId), "Message received: " ++
			   binary_to_list(Text)),
    {noreply, State};
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
