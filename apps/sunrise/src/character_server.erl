-module(character_server).
-behaviour(gen_server).

-export([create/3, character_exists/1, enter/3, bind_character/2, characters_in_world/0, character_by_pid/1, character_location/1, character_location_by_pid/1, move_to/3, check_stat/3, check_stat_easy/2, check_stat_moderate/2, check_stat_hard/2, use_skill/2, use_skill/3, create_npc/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

create(Sender, User, Name, Control) ->
    case character_exists(Name) of
        true ->
            io:format("Attempted to recreate existing character.~n");
        false ->
            io:format("Creating character ~s for user ~s on pid ~p~n", [Name, User, Sender]),
            db:insert(save, characters, {Name, User, nowhere, Control}),
            db:insert(mem, characters_by_pid, {Sender, Name, User, nowhere, Control}),
            stats_server:initialize(Name),
            skills_server:initialize(Name),
            true
    end.
create(Sender, User, Name) ->
    create(Sender, User, Name, player).

create_npc(Sender, Name) ->
    create(Sender, none, Name, ai). 

enter(Sender, User, Name) ->
    case character_exists(Name) of
        true ->
            Location = character_location(Name),
            io:format("[~p] enter world with ~p at ~p~n", [Sender, Name, Location]),
            db:insert(characters_by_pid, {Sender, Name, User, Location}),
            room_server:enter(Name, Location),
            true;
        false ->
            io:format("[~p,~p] failed to enter world~n", [Name, Sender]),
            false
    end.

character_exists(Name) ->
    case db:lookup(mem, characters, Name) of
        [{Name, _, _, _}] -> true;
        _ -> false
    end.

character_location(Name) ->
    case db:lookup(mem, characters, Name) of
        [{Name, _, Location, _}] -> Location;
        _ -> notfound
    end.

character_by_pid(Pid) ->
    case db:lookup(characters_by_pid, Pid) of
        [{Pid, Name, _, _}] -> Name;
        _ -> notfound
    end.

character_location_by_pid(Pid) ->
    case db:lookup(characters_by_pid, Pid) of
        [{Pid, _, _, Location}] -> Location;
        _ -> notfound
    end.

move_to(Character, Pid, Room) ->
    io:format("Moving ~p with pid ~p to ~p~n", [Character, Pid, Room]),
    true = db:update(mem, characters_by_pid, Pid, [{4, Room}]),
    room_server:enter(Character, Room).

check_stat(Character, StatName, Difficulty) ->
    %% stats are 1-100, represent 1% chance per point
    %% 100-Difficulty will be added to stat
    io:format("Checking ~p for ~p~n", [StatName, Character]),
    Value = stats_server:stat_value(Character, StatName),
    ModifiedValue = Value + 100 - Difficulty + rand:uniform(100),
    case ModifiedValue of
        Result when Result > 99 -> pass;
        _ -> fail
    end.

check_stat_easy(Character, StatName) ->
    check_stat(Character, StatName, 25).

check_stat_moderate(Character, StatName) ->
    check_stat(Character, StatName, 50).

check_stat_hard(Character, StatName) ->
    check_stat(Character, StatName, 75).

use_skill(Character, Skill) ->
    case skills_server:find(Character, Skill) of
        [{skill, _, _, _, Stat, BodySystemName, _, _}] -> 
            case stats_server:bodysystem_status(Character, BodySystemName) of
                [healthy] -> check_stat_easy(Character, Stat);
                [degraded] -> check_stat_moderate(Character, Stat);
                [impaired] -> check_stat_hard(Character, Stat);
                [destroyed] -> fail
            end;
        _ -> notfound
    end.
 
use_skill(Character, Skill, Target) ->
    io:format("[~p] Attempting to use ~p on ~p~n", [Character, Skill, Target]),
    case character_exists(Target) of
        false -> notfound;
        true -> 
            case skills_server:find(Character, Skill) of
                [{skill, _, _, _, Stat, BodySystemName, DefenseStat, DamageBS}] -> 
                [BSStatus] = stats_server:bodysystem_status(Character, BodySystemName),
                case check_stat(Character, Stat, 
                    determine_difficulty(BSStatus, Target, DefenseStat)) of
                    fail -> fail;
                    pass ->
                        % Succeeded, do damage
                        case DamageBS of
                            none -> pass;
                            BS ->
                                stats_server:damage(Target, BS),
                                pass
                        end 
                end;
                _ -> notfound
            end
    end.

determine_difficulty(BodySystemStatus, Target, TargetStat) ->
    BodySystemDifficulty = case BodySystemStatus of
        healthy -> 25;
        degraded -> 50;
        impaired -> 75;
        destroyed -> 99
    end,
    TargetStatValue = case stats_server:stat_value(Target, TargetStat) of
        notfound -> 0;
        Val -> Val
    end,
    BodySystemDifficulty + TargetStatValue.

bind_character(Name, Location) ->
    ok = gen_server:cast(?MODULE, {set_bind, Name, Location}).

characters_in_world() ->
    case db:match(characters_by_pid, '$1') of
        [Results] -> charnames(Results);
        _ -> []
    end.

charnames(CharsByPid) ->
    charnames(CharsByPid, []).
charnames([], Charnames) ->
    Charnames;
charnames([{_, Name, _, _}|T], Charnames) ->
    charnames(T, [Name|Charnames]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, characters, [public, named_table], Path),
    db:start(mem, characters_by_pid, [public, named_table]),
    {ok, {}}.

handle_cast({set_bind, Name, Location}, State) ->
    % Upgrade old records
    case db:lookup(mem, characters, Name) of
        [{Name, User}] -> 
            db:insert(save, characters, {Name, User, Location});
        [{Name, User, _}] ->
            db:update(save, characters, {Name, User, Location})
    end,
    io:format("Character ~s now has start location of ~s~n", [Name, Location]),
    {noreply, State};
handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.
