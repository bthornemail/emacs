;;; test-server-client.el --- Test server and client functionality

;; This script tests the server and client components

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load core components first
(message "=== Loading Core Components ===")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-archimedean.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")

(message "✓ Core components loaded successfully")

;; Load server and client
(message "=== Loading Server and Client ===")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-server.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-client.el")

(message "✓ Server and client loaded successfully")

;; Start server
(message "=== Starting Server ===")
(wave-server-start)

;; Connect client
(message "=== Connecting Client ===")
(wave-client-connect)

;; Test server functionality
(message "=== Testing Server Functionality ===")
(wave-server-test-geometric-shapes)
(wave-server-test-church-encoding)
(wave-server-test-wave-creation)

;; Test client functionality
(message "=== Testing Client Functionality ===")
(wave-client-test-all-components)

;; Test server-client communication
(message "=== Testing Server-Client Communication ===")
(wave-client-ping-server)
(wave-client-get-server-status)
(wave-client-test-component "wave-multiplexer")

;; Show status
(message "=== Final Status ===")
(wave-server-send-status)

(message "=== Server-Client Test Complete ===")
(message "Server running: %s" wave-server-running)
(message "Client connected: %s" wave-client-connected)
