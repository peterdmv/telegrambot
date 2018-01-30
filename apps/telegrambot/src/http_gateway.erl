-module(http_gateway).

-behaviour(gen_server).

%% API
-export([reply/2,
	 start_link/1,
	 start_bot/1,
	 webhook_update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {sup,
		token,
	        certfile,
		keyfile,
		chats=#{}}).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Sup) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Sup, []).


start_bot(ChatId) ->
    gen_server:call(?MODULE, {start_bot, ChatId}, infinity).


reply(ChatId, Text) ->
    gen_server:cast(?MODULE, {reply, ChatId, Text}).


webhook_update({Env, In}) ->
    gen_server:cast(?MODULE, {webhook_update, Env, In}).

%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init(Sup) ->
    {ok, Token} = application:get_env(token),
    {ok, CertFile} = application:get_env(certfile),
    {ok, KeyFile} = application:get_env(keyfile),

    % Start inet and httpd
    start_httpd(CertFile, KeyFile),

    % Start Bot Supervisor
    self() ! {start_bot_sup, Sup},
    {ok, #state{token=Token, certfile=CertFile, keyfile=KeyFile}}.

handle_call({start_bot, ChatId}, _,  S = #state{chats=Chats0}) ->
    {ok, Pid} = bot_sup:start_child(ChatId),
    Ref = erlang:monitor(process, Pid),
    Chats = maps:put(ChatId, {Pid, Ref}, Chats0),
    {reply, {ok, Pid}, S#state{chats=Chats}};
handle_call(_, _, State) ->
    {reply, ok, State}.

%---------------------------------------------------------------------------
handle_cast({start_bot, ChatId}, S = #state{chats=Chats0}) ->
    {ok, Pid} = bot_sup:start_child(ChatId),
    Ref = erlang:monitor(process, Pid),
    Chats = maps:put(ChatId, {Pid, Ref}, Chats0),
    {noreply, S#state{chats=Chats}};
%---------------------------------------------------------------------------
handle_cast({reply, ChatId, Text}, State = #state{token=Token}) ->
    do_reply(Token, ChatId, Text),
    {noreply, State};
handle_cast({webhook_update, Env, In}, State = #state{chats=Chats}) ->
    JSON = jsx:decode(list_to_binary(In), [return_maps]),
    Id = get_chat_id(JSON),
    %% io:format("Token: ~p~n", [application:get_env(token)]),
    %% io:format("ChatID: ~p~n", [Id]),
    %% io:format("Webhook update ### Env: ~p~n In: ~p~n JSON> ~p~n", [Env, In, JSON]),
    io:format("TEST MAP: ~p~n",  [maps:get(Id, Chats, newchat)]),
    case maps:get(Id, Chats, newchat) of
	{Pid, _} ->
	    bot:update(Pid, JSON);
    newchat ->
	    init_bot(Id)
	    %% bot:update(Pid, JSON)
    end,
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({start_bot_sup, Sup}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, bot_sup()),
    link(Pid),
    % Start pre-configured bots
    Res = application:get_env(chats),
    io:format("RESs: ~p~n", [Res]),
    {ok, Chats} = Res,
    io:format("Chats: ~p~n", [Chats]),
    [init_bot(ChatId) || ChatId <- Chats],
    {noreply, S#state{sup=Pid}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

init_bot(ChatId) ->
    gen_server:cast(?MODULE, {start_bot, ChatId}).


bot_sup() ->
    #{id => bot_sup,
      start => {bot_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [bot_sup]}.

start_httpd(CertFile, KeyFile) ->
    inets:start(),
    {ok, Pid} =
	inets:start(httpd,
		    [{port, 22443},
		     {server_name,"localhost"},
		     {server_root,"./"},
		     {document_root,"./"},
		     {bind_address, any},
		     {socket_type, {ssl, [{certfile, CertFile},
					  {keyfile, KeyFile}]}},
		     {mimetypes, [{"html", "text/html"}]},
		     {modules,[mod_esi]},
		     {erl_script_alias, {"/", [bot_esi]}}
		    ]),
    Pid.


get_chat_id(JSON) ->
    maps:get(<<"id">>,maps:get(<<"chat">>,maps:get(<<"message">>, JSON))).

do_reply(Token, ChatId, Text) ->
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/sendMessage?chat_id=" ++ ChatId ++
			    "&text=" ++ Text),
    io:format("Res: ~p~n", [Res]).
