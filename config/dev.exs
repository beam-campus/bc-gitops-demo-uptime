import Config

config :demo_uptime, DemoUptimeWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: 8083],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "demo_uptime_dev_secret_key_base_for_development_only_do_not_use_in_production",
  watchers: []

config :logger, :console, format: "[$level] $message\n"
config :phoenix, :stacktrace_depth, 20
config :phoenix, :plug_init_mode, :runtime
