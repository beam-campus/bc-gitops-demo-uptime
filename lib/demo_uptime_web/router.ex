defmodule DemoUptimeWeb.Router do
  use DemoUptimeWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", DemoUptimeWeb do
    pipe_through :api

    get "/", StatsController, :index
    get "/health", StatsController, :health
    get "/info", StatsController, :info
    get "/stats", StatsController, :stats
    get "/stats/memory", StatsController, :memory
    get "/stats/processes", StatsController, :processes
    get "/stats/uptime", StatsController, :uptime
  end
end
