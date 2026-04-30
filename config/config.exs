import Config

# Micro-frontend components for embedding in host applications
config :demo_uptime, :liveview_components, [
  %{
    id: :vm_stats,
    module: DemoUptimeWeb.StatsComponent,
    title: "BEAM VM Stats",
    description: "Real-time BEAM VM statistics including memory, processes, and I/O",
    pubsub: DemoUptime.PubSub,
    topic: "stats:updates",
    update_assign: :stats
  }
]

config :demo_uptime, DemoUptimeWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Phoenix.Endpoint.Cowboy2Adapter,
  render_errors: [
    formats: [json: DemoUptimeWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: DemoUptime.PubSub,
  live_view: [signing_salt: "demo_uptime_lv"]

config :phoenix, :json_library, Jason

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

import_config "#{config_env()}.exs"
