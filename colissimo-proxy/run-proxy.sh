#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Load env file (export all keys)
set -a
[ -f .env ] && . ./.env
set +a

# Use Node from PATH (you already have v22 which is fine)
echo "Starting Colissimo proxy on ${HOST:-127.0.0.1}:${PORT:-3030} ..."
exec node server.js
