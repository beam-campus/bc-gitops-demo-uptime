# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2026-01-13

### Added

- Initial release of demo_uptime
- HTTP endpoints for BEAM VM statistics:
  - `GET /` - Service info and available endpoints
  - `GET /health` - Health check with uptime
  - `GET /info` - Application version and OTP info
  - `GET /stats` - Full VM statistics
  - `GET /stats/memory` - Memory breakdown
  - `GET /stats/processes` - Process statistics
- Real-time BEAM VM metrics:
  - Uptime (wall clock and runtime)
  - Memory breakdown (processes, atoms, binaries, ETS, code)
  - Process count and limits
  - Scheduler and CPU information
  - I/O statistics
  - Garbage collection stats
- JSON responses via jiffy
- HTTP server via cowboy
- Configurable HTTP port (default: 8083)
- bc_gitops integration example for hex.pm deployment
