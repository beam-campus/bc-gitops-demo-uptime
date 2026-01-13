#!/usr/bin/env bash
#
# Publish demo_uptime to hex.pm
#
# Usage: ./scripts/publish_to_hex.sh
#
# Prerequisites:
#   - Elixir/Mix installed
#   - hex authentication configured (mix hex.user auth)
#   - Source secrets: source ~/.config/zshrc/01-secrets
#
set -euo pipefail

# Source secrets if available
if [[ -f ~/.config/zshrc/01-secrets ]]; then
    source ~/.config/zshrc/01-secrets
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

echo "==> Checking prerequisites..."

# Check mix
if ! command -v mix &> /dev/null; then
    echo "ERROR: mix not found. Please install Elixir."
    exit 1
fi

# Check for uncommitted changes
if [[ -n $(git status --porcelain) ]]; then
    echo "ERROR: Uncommitted changes detected. Please commit or stash them first."
    git status --short
    exit 1
fi

echo "==> Getting dependencies..."
mix deps.get

echo "==> Compiling..."
mix compile

echo "==> Running tests..."
mix test || echo "WARNING: Tests failed (continuing anyway)"

echo "==> Building documentation..."
mix docs

echo ""
echo "==> Package ready!"
echo ""
echo "==> Publishing to hex.pm..."
echo ""

mix hex.publish --yes

echo ""
echo "==> Done! Package published to hex.pm"
