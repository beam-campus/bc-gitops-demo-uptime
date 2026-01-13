import Config

config :demo_uptime, DemoUptimeWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 8083],
  check_origin: false,
  server: true,
  secret_key_base: System.get_env("SECRET_KEY_BASE") || "demo_uptime_prod_secret"

config :logger, level: :info
