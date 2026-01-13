%%%-------------------------------------------------------------------
%%% @doc demo_uptime top level supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(demo_uptime_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @doc Start the supervisor.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Supervisor init callback.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
