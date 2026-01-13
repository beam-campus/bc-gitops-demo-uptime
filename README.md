# demo_uptime

[![Hex.pm](https://img.shields.io/hexpm/v/demo_uptime.svg)](https://hex.pm/packages/demo_uptime)
[![Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/demo_uptime)

BEAM VM uptime and statistics service for bc_gitops demos.

This application demonstrates deploying an Erlang service via **hex.pm** (instead of git) using bc_gitops.

## Features

- Real-time BEAM VM statistics
- Memory breakdown (processes, atoms, binaries, ETS, code)
- Process count and limits
- Scheduler and CPU information
- I/O and garbage collection stats
- Formatted uptime display

## Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /` | Service info and available endpoints |
| `GET /health` | Health check with uptime |
| `GET /info` | Application version and OTP info |
| `GET /stats` | Full VM statistics |
| `GET /stats/memory` | Memory breakdown |
| `GET /stats/processes` | Process count and limits |

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {demo_uptime, "0.1.0"}
]}.
```

Or with Mix:

```elixir
def deps do
  [
    {:demo_uptime, "~> 0.1.0"}
  ]
end
```

## Configuration

Default HTTP port is `8083`. Configure via application environment:

```erlang
{demo_uptime, [
    {http_port, 8083}
]}.
```

## bc_gitops Integration

To deploy via bc_gitops using hex.pm as source, create an `app.config`:

```erlang
#{
    name => demo_uptime,
    version => <<"0.1.0">>,
    source => #{
        type => hex
    },
    env => #{
        http_port => 8083
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
rebar3 get-deps

# Compile
rebar3 compile

# Run interactive shell
rebar3 shell

# Test endpoints
curl http://localhost:8083/health
curl http://localhost:8083/stats
```

## License

MIT - See [LICENSE](LICENSE) for details.
