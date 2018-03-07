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
-module(http_gateway).

-behaviour(gen_server).

%% API
-export([answer_query/2,
	 get/1,
	 reply/2, reply/3,
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
		cache=#{},
		chats=#{}}).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Sup) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Sup, []).

start_bot(ChatId) ->
    gen_server:call(?MODULE, {start_bot, ChatId}, infinity).

answer_query(QueryId, Answer) ->
    gen_server:cast(?MODULE, {answer_inline_query, QueryId, Answer}).

get(Uri) ->
    gen_server:call(?MODULE, {get, Uri}, infinity).

reply(ChatId, Text) ->
    gen_server:cast(?MODULE, {reply, ChatId, Text}).

reply(ChatId, Text, keyboard) ->
    gen_server:cast(?MODULE, {reply_with_keyboard, ChatId, Text}).

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


handle_call({get, Uri}, _,  S = #state{cache=Cache0}) ->
    case read_cache(Uri, Cache0) of
	{none, Cache1} ->
	    Answer = httpc:request(get, {Uri, []}, [], [{body_format, string}]),
	    Cache2 = write_cache(Uri, Answer, Cache1),
	    {reply, Answer, S#state{cache=Cache2}};
	{Answer, _} ->
	    {reply, Answer, S}
    end;
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
handle_cast({start_bot, ChatId, Msg}, S = #state{chats=Chats0}) ->
    {ok, Pid} = bot_sup:start_child(ChatId),
    Ref = erlang:monitor(process, Pid),
    Chats = maps:put(ChatId, {Pid, Ref}, Chats0),
    bot:update(Pid, Msg),
    {noreply, S#state{chats=Chats}};
%---------------------------------------------------------------------------
handle_cast({answer_inline_query, QueryId, Answer}, State = #state{token=Token}) ->
    do_answer_inline_query(Token, QueryId, Answer),
    {noreply, State};
handle_cast({reply, ChatId, Text}, State = #state{token=Token}) ->
    do_reply(Token, ChatId, Text),
    {noreply, State};
handle_cast({reply_with_keyboard, ChatId, Text}, State = #state{token=Token}) ->
    do_reply_with_inline_keyboard(Token, ChatId, Text),
    {noreply, State};
handle_cast({webhook_update, _Env, In}, State) ->
    process_message(In, State),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({start_bot_sup, Sup}, S = #state{}) ->
    {ok, Pid0} = supervisor:start_child(Sup, bot_sup()),
    link(Pid0),
    {ok, Pid1} = supervisor:start_child(Sup, query_handler()),
    link(Pid1),

    % Start pre-configured bots
    Res = application:get_env(chats),
    io:format("RESs: ~p~n", [Res]),
    {ok, Chats} = Res,
    io:format("Chats: ~p~n", [Chats]),
    [init_bot(ChatId) || ChatId <- Chats],
    {noreply, S#state{sup=Pid0}};
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

init_bot(ChatId, Msg) ->
    gen_server:cast(?MODULE, {start_bot, ChatId, Msg}).

bot_sup() ->
    #{id => bot_sup,
      start => {bot_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [bot_sup]}.

query_handler() ->
    #{id => query_handler,
      start => {query_handler, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [query_handler]}.

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

%% Sends a custom keyboard with two buttons to the clients
do_reply_with_keyboard(Token, ChatId, Text) ->
    Keyboard = [[ [{<<"text">>, <<"hk">>}], [{<<"text">>, <<"isas">>}] ]],
    ReplyKeyboardMarkup = [{<<"keyboard">>, Keyboard},
			   {<<"resize_keyboard">>, true}],
    ReplyMarkup = jsx:encode(ReplyKeyboardMarkup),
    Query = uri_string:compose_query([{"chat_id", ChatId},
				      {"text", Text},
				      {"reply_markup", ReplyMarkup}]),
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/sendMessage?" ++ Query),
    io:format("Query: ~p~n", [Query]),
    io:format("Res: ~p~n", [Res]).

do_reply_with_inline_keyboard(Token, ChatId, Text) ->
    Keyboard = [[ [{<<"text">>, <<"HK">>}, {<<"callback_data">>, <<"hk">>}],
		  [{<<"text">>, <<"Isas Spis">>}, {<<"callback_data">>, <<"isas">>}] ]],
    ReplyKeyboardMarkup = [{<<"inline_keyboard">>, Keyboard}],
    ReplyMarkup = jsx:encode(ReplyKeyboardMarkup),
    Query = uri_string:compose_query([{"chat_id", ChatId},
				      {"text", Text},
				      {"reply_markup", ReplyMarkup}]),
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/sendMessage?" ++ Query),
    io:format("Query: ~p~n", [Query]),
    io:format("Res: ~p~n", [Res]).

do_reply(Token, ChatId, Text) ->
    Query = uri_string:compose_query([{"chat_id", ChatId},
				      {"text", Text}]),
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/sendMessage?" ++ Query),
    io:format("Query: ~p~n", [Query]),
    io:format("Res: ~p~n", [Res]).

do_answer_inline_query(Token, QueryId, Answer) ->
    Results = jsx:encode(Answer),
    URIQuery = uri_string:compose_query([{"inline_query_id", QueryId},
				      {"results", Results}]),
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/answerInlineQuery?" ++ URIQuery),
    io:format("JSON answer: ~p~n", [Results]),
    io:format("Res: ~p~n", [Res]).

process_message(In, State)  ->
     Json = jsx:decode(list_to_binary(In), [return_maps]),
     process_json(Json, State).

process_json(#{<<"callback_query">> := Cb}, #state{chats=Chats}) ->
    Data = maps:get(<<"data">>, Cb),
    Msg = maps:get(<<"message">>, Cb, none),
    ChatId = maps:get(<<"id">>,maps:get(<<"chat">>, Msg)),
    io:format("### ChatId ~p~n: ", [ChatId]),
    case maps:get(ChatId, Chats, newchat) of
    	{Pid, _} ->
    	    bot:callback_query(Pid, Data);
	newchat ->
    	    init_bot(ChatId)
    end;
process_json(#{<<"message">> := Msg} = JSON, #state{chats=Chats}) ->
    ChatId = maps:get(<<"id">>,maps:get(<<"chat">>, Msg)),
    io:format("### ChatId ~p~n: ", [ChatId]),
    case maps:get(ChatId, Chats, newchat) of
    	{Pid, _} ->
    	    bot:update(Pid, JSON);
	newchat ->
    	    init_bot(ChatId, JSON)
    end;
process_json(#{<<"inline_query">> := InlineQuery}, _) ->
    io:format("Inline Query: ~p~n", [InlineQuery]),
    query_handler:inline_query(InlineQuery);
process_json(Other, _State) ->
    io:format("Unknown message: ~p~n", [Other]).


read_cache(Uri, Cache) ->
    Now = os:system_time(second),
    case maps:get(Uri, Cache, none) of
	{Answer, Timestamp} when (Now - Timestamp) < 1800 ->
	    {Answer, Cache};
	{_, _} ->
	    {none, maps:remove(Uri, Cache)};
	none ->
	    {none, Cache}
    end.

write_cache(Uri, Answer, Cache) ->
    Cache#{Uri => {Answer, os:system_time(second)}}.
