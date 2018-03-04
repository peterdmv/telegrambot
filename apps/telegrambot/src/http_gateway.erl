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
-export([reply/2, reply/3,
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
handle_cast({reply_with_keyboard, ChatId, Text}, State = #state{token=Token}) ->
    do_reply_with_inline_keyboard(Token, ChatId, Text),
    {noreply, State};
handle_cast({webhook_update, _Env, In}, State) ->
    process_message(In, State),
    {noreply, State};
handle_cast({inline_query, Id, FirstName, QueryId, Query},
	    State = #state{token=Token}) ->
    do_answer_inline_query(Token, Id, FirstName, QueryId, Query),
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


do_answer_inline_query(Token, _Id, _FirstName, QueryId, Query) ->
    Answer = prepare_answer(Query),
    Results = jsx:encode(Answer),
    URIQuery = uri_string:compose_query([{"inline_query_id", QueryId},
				      {"results", Results}]),
    Res = httpc:request("https://api.telegram.org/bot" ++ Token ++
		      "/answerInlineQuery?" ++ URIQuery),
    io:format("JSON answer: ~p~n", [Results]),
    io:format("Query: ~p~n", [Query]),
    io:format("Res: ~p~n", [Res]).

prepare_answer(<<"info">>) ->
    [
     [{<<"type">>,<<"article">>},
      {<<"id">>, <<"item1">>},
      {<<"title">>, <<"Wingsuit Flying">>},
      {<<"thumb_url">>, <<"https://upload.wikimedia.org/wikipedia/commons/e/e8/Wingsuit_Flying_in_Massachusetts_%286367634713%29.jpg">>},
      {<<"thumb_width">>, 50},
      {<<"thumb_height">>, 50},
      {<<"description">>, <<"Sunday 13:00 @ Ranhammarsvagen 14">>},
      {<<"input_message_content">>,
       [{<<"message_text">>,<<"*Wingsuit Flying:* Sunday 13:00 @ Ranhammarsvagen 14">>},
	{<<"parse_mode">>,<<"Markdown">>}]}
     ]
    ];
prepare_answer(<<"lunch">>) ->
    %% Keyboard = [[ [{<<"text">>, <<"HK">>}, {<<"callback_data">>, <<"hk">>}],
    %% 		  [{<<"text">>, <<"Isas Spis">>}, {<<"callback_data">>, <<"isas">>}] ]],
    %% InlineKeyboardMarkup = [{<<"inline_keyboard">>, Keyboard}],
    MenuHK = unicode:characters_to_binary(bot_lunch_parser:fetch(hk)),
    MenuIsas = unicode:characters_to_binary(bot_lunch_parser:fetch(isas)),
    [
     [{<<"type">>,<<"article">>},
      {<<"id">>, <<"item1">>},
      {<<"title">>, <<"HK Restaurant">>},
      {<<"thumb_url">>, <<"https://cdn1.iconfinder.com/data/icons/social-messaging-ui-color-shapes/128/eat-circle-orange-512.png">>},
      {<<"thumb_width">>, 50},
      {<<"thumb_height">>, 50},
      {<<"description">>, <<"Lunch menu">>},
      {<<"input_message_content">>,
       [{<<"message_text">>, MenuHK},
	{<<"parse_mode">>,<<"Markdown">>}]}
     ],
     [{<<"type">>,<<"article">>},
      {<<"id">>, <<"item2">>},
      {<<"title">>, <<"Isas Spis">>},
      {<<"thumb_url">>, <<"https://cdn1.iconfinder.com/data/icons/social-messaging-ui-color-shapes/128/eat-circle-orange-512.png">>},
      {<<"thumb_width">>, 50},
      {<<"thumb_height">>, 50},
      {<<"description">>, <<"Lunch menu">>},
      {<<"input_message_content">>,
       [{<<"message_text">>, MenuIsas},
	{<<"parse_mode">>,<<"Markdown">>}]}
     ]
    ];
prepare_answer(_) ->
    [].

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
    	    init_bot(ChatId)
    end;
process_json(#{<<"inline_query">> := InlineQuery}, _State) ->
    io:format("Inline Query: ~p~n", [InlineQuery]),
    Query = maps:get(<<"query">>, InlineQuery),
    QueryId = maps:get(<<"id">>, InlineQuery),
    From = maps:get(<<"from">>, InlineQuery),
    Id = maps:get(<<"id">>, From),
    FirstName = maps:get(<<"first_name">>, From),
    gen_server:cast(?MODULE, {inline_query, Id, FirstName, QueryId, Query});
process_json(Other, _State) ->
    io:format("Unknown message: ~p~n", [Other]).
