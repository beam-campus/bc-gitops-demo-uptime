defmodule DemoUptimeWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :demo_uptime

  @session_options [
    store: :cookie,
    key: "_demo_uptime_key",
    signing_salt: "demo_uptime_salt",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: false

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Jason

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug DemoUptimeWeb.Router
end
