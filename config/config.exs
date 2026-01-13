import Config

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
