#!/usr/bin/env bash
set -euo pipefail

if [ "$(id -un)" != "jay" ]; then
    echo "ERROR: this script must be run as jay, not $(id -un)" >&2
    exit 1
fi

if [ "${1:-}" = "--stop" ]; then
    echo "Stopping DrDr services..."
    sudo systemctl stop drdr-main.service
    sudo systemctl stop drdr-render.service
    echo ""
    echo "Services stopped."
    systemctl status drdr-main.service --no-pager -l 2>&1 || true
    echo ""
    systemctl status drdr-render.service --no-pager -l 2>&1 || true
    exit 0
fi

echo "Restarting DrDr services..."
sudo systemctl restart drdr-main.service drdr-render.service

sleep 2

echo ""
systemctl status drdr-main.service --no-pager -l 2>&1 || true
echo ""
systemctl status drdr-render.service --no-pager -l 2>&1 || true
