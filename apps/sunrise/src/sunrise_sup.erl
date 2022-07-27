%%%-------------------------------------------------------------------
%% @doc sunrise top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sunrise_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    NetSupSpec = {
        net_sup,
        {net_sup, start_link, []},
        permanent, brutal_kill, supervisor, [net_sup]
    },
    UserServerSpec = {
        user_server,
        {user_server, start_link, [[]]},
        permanent, brutal_kill, worker, [user_server]
    },
    CharacterServerSpec = {
        character_server,
        {character_server, start_link, [[]]},
        permanent, brutal_kill, worker, [character_server]
    },
    RoomServerSpec = {
        room_server,
        {room_server, start_link, [[]]},
        permanent, brutal_kill, worker, [room_server]
    },
    StatsServerSpec = {
        stats_server,
        {stats_server, start_link, [[]]},
        permanent, brutal_kill, worker, [stats_server]
    },
    SkillsServerSpec = {
        skills_server,
        {skills_server, start_link, [[]]},
        permanent, brutal_kill, worker, [skills_server]
    },
    ChildSpecs = [
        NetSupSpec,
        UserServerSpec,
        CharacterServerSpec,
        RoomServerSpec,
        StatsServerSpec,
        SkillsServerSpec
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
