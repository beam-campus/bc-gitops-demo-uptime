%%%-------------------------------------------------------------------
%%% @doc demo_uptime application callback module.
%%%
%%% Starts the HTTP server for BEAM VM statistics.
%%% @end
%%%-------------------------------------------------------------------
-module(demo_uptime_app).
-behaviour(application).

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Start the application.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = application:get_env(demo_uptime, http_port, 8083),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", demo_uptime_handler, []},
            {"/health", demo_uptime_handler, []},
            {"/info", demo_uptime_handler, []},
            {"/stats", demo_uptime_handler, []},
            {"/stats/memory", demo_uptime_handler, []},
            {"/stats/processes", demo_uptime_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(
        demo_uptime_http,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    logger:info("[demo_uptime] HTTP server started on port ~p", [Port]),
    demo_uptime_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the application.
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(demo_uptime_http),
    ok.
