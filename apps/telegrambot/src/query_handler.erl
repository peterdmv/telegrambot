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
-module(query_handler).

-behaviour(gen_server).

%% API
-export([inline_query/1, start_link/0]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {}).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

inline_query(Query) ->
    gen_server:cast(?MODULE, {inline_query, Query}).

%%%=========================================================================
%%%  Gen_server callbacks
%%%=========================================================================
init(_) ->
    {ok, #state{}}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({inline_query, Query}, State) ->
    handle_query(Query),
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

handle_query(InlineQuery) ->
    io:format("Inline Query: ~p~n", [InlineQuery]),
    Query = maps:get(<<"query">>, InlineQuery),
    QueryId = maps:get(<<"id">>, InlineQuery),
    %% From = maps:get(<<"from">>, InlineQuery),
    %% Id = maps:get(<<"id">>, From),
    %% FirstName = maps:get(<<"first_name">>, From),
    Answer = prepare_answer(Query),
    http_gateway:answer_query(QueryId, Answer).

prepare_answer(<<"bullshit">>) ->
    BS = cbsg:sentences(5),
    Short = binary:part(BS, {0, 30}),
    [
     [{<<"type">>,<<"article">>},
      {<<"id">>, <<"item1">>},
      {<<"title">>, <<"Random Corporate Bullshit">>},
      {<<"thumb_url">>, <<"https://store.gocomics.com/wp-content/uploads/2017/10/Dilbert-Pin.jpg">>},
      {<<"thumb_width">>, 50},
      {<<"thumb_height">>, 50},
      {<<"description">>, Short},
      {<<"input_message_content">>,
       [{<<"message_text">>, BS},
	{<<"parse_mode">>,<<"Markdown">>}]}
     ]
    ];
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
