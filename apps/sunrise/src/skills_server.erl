-module(skills_server).
-behaviour(gen_server).

-export([initialize/1, list/1, find/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(skillid, {name, character}).
-record(skill, {id, description, musttarget, stat, bodysystem, defendwith, damages}).

initialize(Character) ->
    db:insert(save, skills, #skill{
        id=#skillid{name=hit, character=Character},
        description="Hit the target with equipped weapon or fist",
        musttarget=false,
        stat=strength,
        bodysystem=rightarm,
        defendwith=constitution,
        damages=rightarm
    }).

list(Character) ->
    db:match(skills, #skill{
        id=#skillid{name='$1', character=Character},
        description='$2',
        musttarget='$3',
        stat='$4',
        bodysystem='$5',
        defendwith='$6',
        damages='$7'
    }).

find(Character, Skill) ->
    db:lookup(skills, #skillid{name=Skill, character=Character}).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, skills, [public, named_table, {keypos, 2}], Path),
    {ok, {}}.

handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.
