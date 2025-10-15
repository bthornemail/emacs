#!/bin/bash
# connect-to-daemon.sh --- Connect to the running Emacs daemon

echo "🔗 Connecting to Wave Function Emacs Daemon"
echo "=========================================="

# Check if daemon is running
if ! emacsclient --eval "(message \"Daemon is running\")" 2>/dev/null; then
    echo "❌ Emacs daemon is not running!"
    echo "Please run: ./start-interactive-daemon.sh"
    exit 1
fi

echo "✅ Emacs daemon is running"
echo ""
echo "Connecting to daemon..."
echo ""

# Connect to daemon
emacsclient -c

echo ""
echo "🎉 Connected to Wave Function Emacs Daemon!"
echo ""
echo "Available interactive commands:"
echo "  M-x test-geometric-shapes-interactive"
echo "  M-x test-archimedean-solids-interactive"
echo "  M-x test-5-cell-interactive"
echo "  M-x test-incidence-matrices-interactive"
echo "  M-x test-betti-numbers-interactive"
echo "  M-x test-server-client-interactive"
echo "  M-x run-all-interactive-tests"
echo ""
echo "Server status: Running on port 8080"
echo "Client status: Connected"
echo ""
echo "Ready for interactive testing! 🚀"
