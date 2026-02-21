#!/usr/bin/env bash
set -euo pipefail

if [ "$(id -un)" != "jay" ]; then
    echo "ERROR: this script must be run as jay, not $(id -un)" >&2
    exit 1
fi

echo "=== DrDr systemd uninstall ==="

# Stop
echo "Stopping services..."
sudo systemctl stop drdr.target 2>/dev/null || true
sudo systemctl stop drdr-main.service 2>/dev/null || true
sudo systemctl stop drdr-render.service 2>/dev/null || true

# Disable
echo "Disabling services..."
sudo systemctl disable drdr-main.service 2>/dev/null || true
sudo systemctl disable drdr-render.service 2>/dev/null || true
sudo systemctl disable drdr.target 2>/dev/null || true

# Remove linked unit files
echo "Removing unit file links..."
sudo rm -f /etc/systemd/system/drdr-main.service
sudo rm -f /etc/systemd/system/drdr-render.service
sudo rm -f /etc/systemd/system/drdr.target

# Clear failed state
sudo systemctl reset-failed drdr-main.service 2>/dev/null || true
sudo systemctl reset-failed drdr-render.service 2>/dev/null || true

# Reload
sudo systemctl daemon-reload

echo ""
echo "=== Done ==="
echo "DrDr systemd units removed."
echo ""
echo "To restart with the old shell script:"
echo "  cd /opt/svn/drdr && ./good-init.sh"
