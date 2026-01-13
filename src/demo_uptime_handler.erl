%%%-------------------------------------------------------------------
%%% @doc HTTP handler for BEAM VM statistics.
%%%
%%% Endpoints:
%%% - GET /         - Welcome message
%%% - GET /health   - Health check
%%% - GET /info     - Application info
%%% - GET /stats    - Full VM statistics
%%% - GET /stats/memory    - Memory breakdown
%%% - GET /stats/processes - Process statistics
%%% @end
%%%-------------------------------------------------------------------
-module(demo_uptime_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc Handle HTTP requests.
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {StatusCode, Body} = handle_request(Method, Path),
    Req = cowboy_req:reply(
        StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req0
    ),
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_request(<<"GET">>, <<"/">>) ->
    Response = #{
        <<"service">> => <<"demo_uptime">>,
        <<"description">> => <<"BEAM VM uptime and statistics service">>,
        <<"endpoints">> => [
            <<"/health">>,
            <<"/info">>,
            <<"/stats">>,
            <<"/stats/memory">>,
            <<"/stats/processes">>
        ]
    },
    {200, jiffy:encode(Response)};

handle_request(<<"GET">>, <<"/health">>) ->
    {Uptime, _} = erlang:statistics(wall_clock),
    Response = #{
        <<"status">> => <<"healthy">>,
        <<"uptime_ms">> => Uptime
    },
    {200, jiffy:encode(Response)};

handle_request(<<"GET">>, <<"/info">>) ->
    {ok, Version} = application:get_key(demo_uptime, vsn),
    Response = #{
        <<"app">> => <<"demo_uptime">>,
        <<"version">> => list_to_binary(Version),
        <<"description">> => <<"BEAM VM uptime and statistics service">>,
        <<"otp_release">> => list_to_binary(erlang:system_info(otp_release)),
        <<"erts_version">> => list_to_binary(erlang:system_info(version))
    },
    {200, jiffy:encode(Response)};

handle_request(<<"GET">>, <<"/stats">>) ->
    Response = #{
        <<"uptime">> => get_uptime(),
        <<"memory">> => get_memory_stats(),
        <<"processes">> => get_process_stats(),
        <<"system">> => get_system_info(),
        <<"io">> => get_io_stats(),
        <<"gc">> => get_gc_stats()
    },
    {200, jiffy:encode(Response)};

handle_request(<<"GET">>, <<"/stats/memory">>) ->
    Response = get_memory_stats(),
    {200, jiffy:encode(Response)};

handle_request(<<"GET">>, <<"/stats/processes">>) ->
    Response = get_process_stats(),
    {200, jiffy:encode(Response)};

handle_request(_, _) ->
    Response = #{<<"error">> => <<"Not found">>},
    {404, jiffy:encode(Response)}.

%%--------------------------------------------------------------------
%% Statistics helpers
%%--------------------------------------------------------------------

get_uptime() ->
    {WallClock, _} = erlang:statistics(wall_clock),
    {Runtime, _} = erlang:statistics(runtime),

    Seconds = WallClock div 1000,
    Days = Seconds div 86400,
    Hours = (Seconds rem 86400) div 3600,
    Minutes = (Seconds rem 3600) div 60,
    Secs = Seconds rem 60,

    #{
        <<"wall_clock_ms">> => WallClock,
        <<"runtime_ms">> => Runtime,
        <<"formatted">> => format_uptime(Days, Hours, Minutes, Secs),
        <<"days">> => Days,
        <<"hours">> => Hours,
        <<"minutes">> => Minutes,
        <<"seconds">> => Secs
    }.

format_uptime(0, H, M, S) ->
    iolist_to_binary(io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S]));
format_uptime(D, H, M, S) ->
    iolist_to_binary(io_lib:format("~Bd ~2..0B:~2..0B:~2..0B", [D, H, M, S])).

get_memory_stats() ->
    Memory = erlang:memory(),
    #{
        <<"total_bytes">> => proplists:get_value(total, Memory),
        <<"processes_bytes">> => proplists:get_value(processes, Memory),
        <<"processes_used_bytes">> => proplists:get_value(processes_used, Memory),
        <<"system_bytes">> => proplists:get_value(system, Memory),
        <<"atom_bytes">> => proplists:get_value(atom, Memory),
        <<"atom_used_bytes">> => proplists:get_value(atom_used, Memory),
        <<"binary_bytes">> => proplists:get_value(binary, Memory),
        <<"code_bytes">> => proplists:get_value(code, Memory),
        <<"ets_bytes">> => proplists:get_value(ets, Memory),
        <<"total_mb">> => round_to(proplists:get_value(total, Memory) / 1048576, 2)
    }.

get_process_stats() ->
    #{
        <<"count">> => erlang:system_info(process_count),
        <<"limit">> => erlang:system_info(process_limit),
        <<"usage_percent">> => round_to(
            erlang:system_info(process_count) / erlang:system_info(process_limit) * 100, 2
        )
    }.

get_system_info() ->
    #{
        <<"otp_release">> => list_to_binary(erlang:system_info(otp_release)),
        <<"erts_version">> => list_to_binary(erlang:system_info(version)),
        <<"schedulers">> => erlang:system_info(schedulers),
        <<"schedulers_online">> => erlang:system_info(schedulers_online),
        <<"logical_processors">> => erlang:system_info(logical_processors),
        <<"atom_count">> => erlang:system_info(atom_count),
        <<"atom_limit">> => erlang:system_info(atom_limit),
        <<"port_count">> => erlang:system_info(port_count),
        <<"port_limit">> => erlang:system_info(port_limit),
        <<"ets_count">> => length(ets:all()),
        <<"node">> => atom_to_binary(node(), utf8)
    }.

get_io_stats() ->
    {{input, Input}, {output, Output}} = erlang:statistics(io),
    #{
        <<"input_bytes">> => Input,
        <<"output_bytes">> => Output,
        <<"input_mb">> => round_to(Input / 1048576, 2),
        <<"output_mb">> => round_to(Output / 1048576, 2)
    }.

get_gc_stats() ->
    {NumberOfGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    #{
        <<"number_of_gcs">> => NumberOfGCs,
        <<"words_reclaimed">> => WordsReclaimed
    }.

round_to(Number, Decimals) ->
    P = math:pow(10, Decimals),
    round(Number * P) / P.
