-module(bot_lunch_parser).

-export([fetch/1]).
-compile(export_all).


fetch(factory) ->
    "Seriously? May I recommend another restaurant?";

fetch(isas) ->
    {ok, {_,_, Body}} = http_gateway:get("http://www.rosiescatering.se/isas-meny-31684489"),
    Tokens = mochiweb_html:parse(Body),

    %% TokenList = [{Tag, Attribs, Value}

    Today = day_of_the_week(),
    Menu = parse_food(Tokens),
    Header = make_header("Isas Spis", Today),
    Header ++ maps:get(day_of_the_week(Today), Menu);


fetch(hk) ->
    {ok, {_,_, Body}} = http_gateway:get("http://hk.kvartersmenyn.se/"),
    Tokens = mochiweb_html:parse(Body),

    %% TokenList = [{Tag, Attribs, Value}

    Today = day_of_the_week(),
    Menu = parse_food(Tokens),
    Header = make_header("HK", Today),
    Header ++ maps:get(day_of_the_week(Today), Menu);


fetch(ericofood) ->
    {ok, {_,_, Body}} = http_gateway:get("http://foodbycoor.se/restauranger/ericsson/ericofood/lunch/"),
    Tokens = mochiweb_html:parse(Body),

    %% TokenList = [{Tag, Attribs, Value}

    Today = day_of_the_week(),
    Menu = parse_food(Tokens),
    Header = make_header("Ericofood", Today),
    Header ++ maps:get(day_of_the_week(Today), Menu);

fetch(_) ->
    "Not implemented".


make_header(Restaurant, Day) ->
    Restaurant ++ " (" ++ day_of_the_week(Day) ++ "): ".


parse_food({_,_,L}) ->
    parse_food(L, #{}).
%%
parse_food([], Map) ->
    add_weekend_menu(Map);
parse_food([{_,_,V}|T], Map) ->
    parse_food(V ++ T, Map);
parse_food([<<"Måndag"/utf8,_/binary>>|R], Map) ->
    parse_day("Monday", R, Map);
parse_food({_, _, R}, Map) ->
    parse_food(R, Map);
parse_food([_|R], Map) ->
    parse_food(R, Map).

parse_day(Day, Tokens, Map) ->
    parse_day(Day, Tokens, Map, []).
%%
parse_day(_, [], Map, []) ->
    add_weekend_menu(Map);
parse_day(Day, [{_,_,V}|T], Map, []) ->
    parse_day(Day, V ++ T, Map, []);
parse_day(Day, [H|R], Map, Acc) ->
    case H of
        {_,_} ->
	    %% Drop comments
	    parse_day(Day, R, Map, Acc);
	{_,_,V} ->
	    parse_day(Day, V ++ R, Map, Acc);
	<<"Måndag"/utf8,_/binary>> ->
	    parse_day("Monday", R, Map#{Day => frev(Acc)}, []);
	<<"Tisdag"/utf8,_/binary>> ->
	    parse_day("Tuesday", R, Map#{Day => frev(Acc)}, []);
	<<"Onsdag"/utf8,_/binary>> ->
	    parse_day("Wednesday", R, Map#{Day => frev(Acc)}, []);
	<<"Torsdag"/utf8,_/binary>> ->
	    parse_day("Thursday", R, Map#{Day => frev(Acc)}, []);
	<<"Fredag"/utf8,_/binary>> ->
	    parse_day("Friday", R, Map#{Day => frev(Acc)}, []);
        B when is_binary(B), length(Acc) > 5 ->
	    parse_day("", [], Map#{Day => frev(Acc)}, []);
	B when is_binary(B) ->
	    L = strip(unicode:characters_to_list(B)),
	    parse_day(Day, R, Map, [L|Acc])
    end.

add_weekend_menu(Map) ->
    Map#{"Saturday" => "Closed!",
	 "Sunday" => "Closed!"}.

frev(L) ->
    frev(L, []).
%%
frev([], Acc) ->
    lists:flatten(Acc);
frev([H|T], Acc) ->
    case is_valid_food(H) of
	true ->
	    frev(T, [H ++ " *** "|Acc]);
	false ->
	    frev(T, Acc)
    end.

is_valid_food(L) ->
    is_valid_food(L, 3).
%%
is_valid_food(_, 0) ->
    true;
is_valid_food([], _) ->
    false;
is_valid_food([H|T], N)
  when $a =< H, H =< $z;
       $A =< H, H =< $Z ->
    is_valid_food(T, N - 1);
is_valid_food([_|T], N) ->
    is_valid_food(T, N).




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

day_of_the_week() ->
    {Date, _} = calendar:local_time(),
    calendar:day_of_the_week(Date).
%%
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
