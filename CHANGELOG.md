# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.1] - 2026-01-13

### Fixed

- Fix KeyError in StatsComponent.mount/1 - `:id` assign is not available during mount, moved refresh scheduling to update/2

## [0.2.0] - 2026-01-13

### Changed

- **BREAKING**: Rewritten from Erlang to Elixir/Phoenix
- Now uses Phoenix LiveView for the embeddable component

### Added

- `DemoUptimeWeb.StatsComponent` - embeddable LiveComponent for host applications
  - Real-time uptime display with days/hours/minutes/seconds
  - Memory usage with breakdown (processes, binary, ETS)
  - Process count with visual usage bar
  - Scheduler/CPU information
  - I/O statistics
  - GC stats and system info
  - Theme support (light/dark)
- New endpoint: `GET /stats/uptime` for uptime-only queries
- Phoenix PubSub support for real-time updates

### Removed

- Erlang/rebar3 build system (replaced with Mix)
- jiffy JSON library (replaced with Jason)

## [0.1.0] - 2026-01-13

### Added

- Initial release of demo_uptime (Erlang version)
- HTTP endpoints for BEAM VM statistics:
  - `GET /` - Service info and available endpoints
  - `GET /health` - Health check with uptime
  - `GET /info` - Application version and OTP info
  - `GET /stats` - Full VM statistics
  - `GET /stats/memory` - Memory breakdown
  - `GET /stats/processes` - Process statistics
- Real-time BEAM VM metrics
- JSON responses via jiffy
- HTTP server via cowboy
- Configurable HTTP port (default: 8083)
- bc_gitops integration example for hex.pm deployment
