-module(hs_tracker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, parser/0, my_time/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-record(player, {name,
                 id,
                 hero,
                 result}).
-record(game, {start_ts :: string(),
               end_ts :: string(),
               player1,
               player2}).
-record(result, {win,lose}).

-define(TYPE_CREATE_GAME, <<"CREATE_GAME">>).
-define(TYPE_FULL_ENTITY, <<"FULL_ENTITY">>).
-define(TYPE_TAG_CHANGE, <<"TAG_CHANGE">>).
-define(SOURCE_VALID, <<"GameState.DebugPrintPower()">>).
-define(MY_USERNAME, <<"Entity=terror">>).
-define(LOG_PATH, "/Applications/Hearthstone/Logs/Power.log").

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

contains(Src, Bstrs) ->
    case binary:match(Src, Bstrs) of
        nomatch -> false;
        _ -> true
    end.

%% parse full entity for hero class
parse_full_entity([_, <<"Creating">>, HeroID, CardID], [Cur|Rest]) ->
    P1 = Cur#game.player1,
    [<<"ID">>, ID] = binary:split(HeroID, [<<"=">>], [global]),
    case contains(CardID, <<"HERO_">>) of
        true ->
            case P1 of
                undefined ->
                    [Cur#game{player1=#player{hero=CardID, id=ID}}|Rest];
                _ ->
                    [Cur#game{player2=#player{hero=CardID, id=ID}}|Rest]
            end;
        false -> [Cur|Rest]
    end;

parse_full_entity(_, State) ->
    State.

%% parse tag_change for player name and game result
parse_tag_change([Time, PlayerName, <<"tag=HERO_ENTITY">>, HeroID], [Cur|Rest]) ->
    P1 = Cur#game.player1,
    [<<"value">>, ID] = binary:split(HeroID, [<<"=">>], [global]),
    case P1#player.id of
        ID -> [Cur#game{player1=(Cur#game.player1)#player{name=PlayerName}}|Rest];
        _ -> [Cur#game{player2=(Cur#game.player2)#player{name=PlayerName}}|Rest]
    end;

parse_tag_change([Time, PlayName, <<"tag=PLAYSTATE">>, <<"value=LOST">>], [Cur|Rest]) ->
    case (Cur#game.player1)#player.name of
        PlayName ->
            [Cur#game{player1=(Cur#game.player1)#player{result = <<"LOST">>},
                      player2=(Cur#game.player2)#player{result = <<"WON">>},
                      end_ts=Time}|Rest];
        _ELSE ->
            [Cur#game{player2=(Cur#game.player2)#player{result = <<"LOST">>},
                      player1=(Cur#game.player1)#player{result = <<"WON">>},
                      end_ts=Time}|Rest]
    end;
parse_tag_change(_, State) -> State.

%%
parse_by_type([?TYPE_CREATE_GAME, Time, Rest], State) ->
    [#game{start_ts=Time, end_ts=undefined, player1=undefined, player2=undefined}|State];
parse_by_type([?TYPE_FULL_ENTITY, Time, Rest], State) ->
    parse_full_entity(Rest, State);
parse_by_type([?TYPE_TAG_CHANGE, Time, Rest], State) ->
    parse_tag_change([Time|Rest], State);
parse_by_type(_, State) -> State.

%%
parse_splits([_, Time, ?SOURCE_VALID, _, Type | Rest], State) ->
    parse_by_type([Type, Time, Rest], State);
parse_splits(_, State) ->
    State.

parse_line(Line, State) ->
    parse_splits(binary:split(Line, [<<" ">>], [global]), State).

start_parse([], State) ->
    State;

start_parse([Line|Rest], State) ->
    start_parse(Rest, parse_line(Line, State)).

get_hero(<<"CardID=HERO_01">>) -> <<"Warrior">>;
get_hero(<<"CardID=HERO_01a">>) -> <<"Warrior">>;
get_hero(<<"CardID=HERO_02">>) -> <<"Shaman">>;
get_hero(<<"CardID=HERO_03">>) -> <<"Rogue">>;
get_hero(<<"CardID=HERO_04">>) -> <<"Paladin">>;
get_hero(<<"CardID=HERO_05">>) -> <<"Hunter">>;
get_hero(<<"CardID=HERO_05a">>) -> <<"Hunter">>;
get_hero(<<"CardID=HERO_06">>) -> <<"Druid">>;
get_hero(<<"CardID=HERO_07">>) -> <<"Warlock">>;
get_hero(<<"CardID=HERO_07a">>) -> <<"Warlock">>;
get_hero(<<"CardID=HERO_08">>) -> <<"Mage">>;
get_hero(<<"CardID=HERO_08a">>) -> <<"Mage">>;
get_hero(<<"CardID=HERO_09">>) -> <<"Priest">>;
get_hero(<<"CardID=HERO_09a">>) -> <<"Priest">>.

print_result([], Win, Total) ->
    io:format("~p/~p, Winrate: ~p\n", [Win, Total, Win/Total]),
    ok;
print_result([Game|Rest], Win, Total) ->
    [Me, Op] = case (Game#game.player1)#player.name of
                   ?MY_USERNAME -> [Game#game.player1, Game#game.player2];
                   _ELSE -> [Game#game.player2, Game#game.player1]
               end,
    io:format("~p - ~p: ~p\n", [get_hero(Me#player.hero), get_hero(Op#player.hero), Me#player.result]),
    Win1 = case Me#player.result of
               <<"WON">> -> Win + 1;
               _ -> Win
           end,
    print_result(Rest, Win1, Total+1).

my_time() ->
    {{Y,M,D},{H,MM,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b", [Y, M, D, H, MM, S])).

parser() ->
    Data = readlines(?LOG_PATH),
    Games = start_parse(Data, []),
    io:format("~p\n", [my_time()]),
    print_result(Games, 0, 0).

start(_StartType, _StartArgs) ->
    parser().

stop(_State) ->
    ok.
