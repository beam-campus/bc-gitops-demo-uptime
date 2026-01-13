defmodule DemoUptime.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Phoenix.PubSub, name: DemoUptime.PubSub},
      DemoUptime.StatsCollector,
      DemoUptimeWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: DemoUptime.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    DemoUptimeWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
