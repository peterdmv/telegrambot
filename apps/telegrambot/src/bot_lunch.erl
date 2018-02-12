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
-module(bot_lunch).

-export([fetch/1]).

fetch(isas) ->
    {ok, {_,_, Body}} = httpc:request(get, {"http://www.rosiescatering.se/isas-meny-31684489", []}, [], [{body_format, string}]),
    Tokens = mochiweb_html:parse(Body),
    %% TokenList

    TokenList = mochiweb_xpath:execute("/html/body/div[5]/div[2]/div[1]/div[1]/div[1]/div[4]/div[2]/div[1]/div[1]/div[1]/node()", Tokens),
    parse_food(day_of_the_week(), TokenList);
fetch(_) ->
    "Not implemented".


day_of_the_week() ->
    {Date, _} = calendar:local_time(),
    calendar:day_of_the_week(Date).


parse_food(Day, Tokens) ->
    Prefix = "Isas Spis (" ++ day_of_the_week(Day) ++ "): ",
    Prefix ++ parse_food(Day, Tokens, []).
%%
parse_food(1, [{_,_,[<<"Måndag"/utf8,_/binary>>]}|Tokens], Acc) ->
    parse_food_items(Tokens, Acc);
parse_food(2, [{_,_,[<<"Tisdag"/utf8,_/binary>>]}|Tokens], Acc) ->
    parse_food_items(Tokens, Acc);
parse_food(3, [{_,_,[<<"Onsdag"/utf8,_/binary>>]}|Tokens], Acc) ->
    parse_food_items(Tokens, Acc);
parse_food(4, [{_,_,[<<"Torsdag"/utf8,_/binary>>]}|Tokens], Acc) ->
    parse_food_items(Tokens, Acc);
parse_food(5, [{_,_,[<<"Fredag"/utf8,_/binary>>]}|Tokens], Acc) ->
    parse_food_items(Tokens, Acc);
parse_food(Day, [{_,_,_}|_Tokens], _Acc)
  when Day =:= 6; Day =:= 7 ->
    "Closed";
parse_food(Day, [<<"\nÂ ">>|Tokens], Acc) ->
    parse_food(Day, Tokens, Acc);
parse_food(Day, [_|Tokens], Acc) ->
    parse_food(Day, Tokens, Acc).


parse_food_items([H|Tokens], Acc) ->
    case H of
	{_,_, [<<71,82,195,150,78,84,_/binary>>]} ->
	    parse_food_vegetarian(Tokens, Acc);
	{_,_,_} ->
	    parse_food_items(Tokens, Acc);
	B when is_binary(B) ->
	    L = strip(unicode:characters_to_list(B)) ++ " *** ",
	    parse_food_items(Tokens, [L|Acc])
    end.


parse_food_vegetarian([B|_], Acc) ->
    L = "Veg: " ++ strip(unicode:characters_to_list(B)),
    lists:flatten(lists:reverse([L|Acc])).

strip(L0) ->
    L = string:strip(L0),
    case L of
	[$\n|T] ->
	    strip(T);
	[$:|T] ->
	    strip(T);
	_ ->
	    L
    end.

day_of_the_week(1) ->
    "Monday";
day_of_the_week(2) ->
    "Tuesday";
day_of_the_week(3) ->
    "Wednesday";
day_of_the_week(4) ->
    "Thursday";
day_of_the_week(5) ->
    "Friday";
day_of_the_week(6) ->
    "Saturday";
day_of_the_week(7) ->
    "Sunday".
