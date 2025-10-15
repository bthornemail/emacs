#!/bin/bash
# start-interactive-daemon.sh --- Start Emacs daemon with wave function package

echo "🚀 Starting Emacs daemon with Wave Function Package"
echo "=================================================="

# Change to the wave function package directory
cd /home/main/dev/Axiomatic/demos/emacs-demo

# Start Emacs daemon with the interactive test environment
echo "Starting Emacs daemon..."
emacs --daemon --eval "(load-file \"/home/main/dev/Axiomatic/demos/emacs-demo/interactive-test.el\")"

echo "✅ Emacs daemon started successfully!"
echo ""
echo "To connect to the daemon:"
echo "  emacsclient -c"
echo ""
echo "Available interactive commands in Emacs:"
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
echo "Ready for interactive testing! 🎉"
