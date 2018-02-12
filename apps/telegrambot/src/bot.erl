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
    case binary:match(Text, <<"bullshit">>) of
	nomatch ->
	    noop;
	{_, _} ->
	    http_gateway:reply(integer_to_list(ChatId),
			       binary_to_list(cbsg:sentences(5)))
    end,
    case binary:match(Text, <<"isas">>) of
	nomatch ->
	    noop;
	{_, _} ->
	    http_gateway:reply(integer_to_list(ChatId),
			       bot_lunch:fetch(isas))
    end,

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
