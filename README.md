# demo_uptime

[![Hex.pm](https://img.shields.io/hexpm/v/demo_uptime.svg)](https://hex.pm/packages/demo_uptime)
[![Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/demo_uptime)

BEAM VM uptime and statistics service with embeddable LiveComponent.

This application demonstrates deploying an Elixir/Phoenix service via **hex.pm** using bc_gitops, with a LiveComponent that can be embedded in host applications.

## Features

- Real-time BEAM VM statistics
- Memory breakdown (processes, atoms, binaries, ETS, code)
- Process count and limits
- Scheduler and CPU information
- I/O and garbage collection stats
- Formatted uptime display
- **Embeddable LiveComponent** for host application dashboards

## Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /` | Service info and available endpoints |
| `GET /health` | Health check with uptime |
| `GET /info` | Application version and OTP info |
| `GET /stats` | Full VM statistics |
| `GET /stats/memory` | Memory breakdown |
| `GET /stats/processes` | Process count and limits |
| `GET /stats/uptime` | Uptime information |

## Installation

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:demo_uptime, "~> 0.2.0"}
  ]
end
```

## Configuration

Default HTTP port is `8083`. Configure via application environment:

```elixir
config :demo_uptime, DemoUptimeWeb.Endpoint,
  http: [port: 8083]
```

## Embeddable LiveComponent

The `DemoUptimeWeb.StatsComponent` can be embedded in host applications:

```elixir
<.live_component
  module={DemoUptimeWeb.StatsComponent}
  id="uptime-stats"
  host_app="my_app"
  theme="dark"
/>
```

### Component Features

- Live uptime counter with days/hours/minutes/seconds
- Memory usage with breakdown (processes, binary, ETS)
- Process count with usage bar
- Scheduler/CPU information
- I/O statistics
- GC stats and system info
- Theme support (light/dark)

## bc_gitops Integration

To deploy via bc_gitops using hex.pm as source, create an `app.config`:

```erlang
#{
    name => demo_uptime,
    version => <<"0.2.0">>,
    source => #{
        type => hex
    },
    env => #{
        http_port => 8083,
        liveview_components => [
            #{
                id => vm_stats,
                module => 'Elixir.DemoUptimeWeb.StatsComponent',
                title => <<"BEAM Stats">>,
                description => <<"Real-time BEAM VM statistics">>
            }
        ]
    },
    health => #{
        type => http,
        port => 8083,
        path => <<"/health">>
    }
}.
```

## Example Response

`GET /stats`:

```json
{
  "uptime": {
    "wall_clock_ms": 3661234,
    "runtime_ms": 12345,
    "formatted": "01:01:01",
    "days": 0,
    "hours": 1,
    "minutes": 1,
    "seconds": 1
  },
  "memory": {
    "total_bytes": 45678912,
    "total_mb": 43.56,
    "processes_bytes": 12345678,
    "system_bytes": 33333234,
    "atom_bytes": 1234567,
    "binary_bytes": 2345678,
    "code_bytes": 8901234,
    "ets_bytes": 567890
  },
  "processes": {
    "count": 89,
    "limit": 262144,
    "usage_percent": 0.03
  },
  "system": {
    "otp_release": "27",
    "erts_version": "15.0",
    "schedulers": 8,
    "schedulers_online": 8,
    "logical_processors": 8,
    "node": "nonode@nohost"
  },
  "io": {
    "input_bytes": 12345678,
    "output_bytes": 2345678,
    "input_mb": 11.77,
    "output_mb": 2.24
  },
  "gc": {
    "number_of_gcs": 1234,
    "words_reclaimed": 5678901
  }
}
```

## Development

```bash
# Get dependencies
mix deps.get

# Compile
mix compile

# Run server
mix phx.server

# Test endpoints
curl http://localhost:8083/health
curl http://localhost:8083/stats
```

## License

MIT - See [LICENSE](LICENSE) for details.
