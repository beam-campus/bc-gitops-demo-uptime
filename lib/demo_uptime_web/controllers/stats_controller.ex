defmodule DemoUptimeWeb.StatsController do
  use DemoUptimeWeb, :controller

  alias DemoUptime.Stats

  def index(conn, _params) do
    json(conn, %{
      service: "demo_uptime",
      description: "BEAM VM uptime and statistics service",
      endpoints: [
        "/health",
        "/info",
        "/stats",
        "/stats/memory",
        "/stats/processes",
        "/stats/uptime"
      ]
    })
  end

  def health(conn, _params) do
    uptime = Stats.uptime()

    json(conn, %{
      status: "healthy",
      uptime_ms: uptime.wall_clock_ms
    })
  end

  def info(conn, _params) do
    {:ok, vsn} = :application.get_key(:demo_uptime, :vsn)

    json(conn, %{
      app: "demo_uptime",
      version: List.to_string(vsn),
      description: "BEAM VM uptime and statistics service",
      otp_release: :erlang.system_info(:otp_release) |> List.to_string(),
      erts_version: :erlang.system_info(:version) |> List.to_string()
    })
  end

  def stats(conn, _params) do
    json(conn, Stats.all())
  end

  def memory(conn, _params) do
    json(conn, Stats.memory())
  end

  def processes(conn, _params) do
    json(conn, Stats.processes())
  end

  def uptime(conn, _params) do
    json(conn, Stats.uptime())
  end
end
