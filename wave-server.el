;;; wave-server.el --- Server for wave function package testing

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: server, testing, wave-function

;;; Commentary:
;; Server component for testing the wave function package.
;; Provides a simple HTTP-like server for interactive testing.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Server configuration
(defvar wave-server-port 8080
  "Port for the wave function server")

(defvar wave-server-running nil
  "Whether the server is currently running")

(defvar wave-server-clients nil
  "List of connected clients")

;; Server state
(defvar wave-server-state (make-hash-table :test 'equal)
  "Server state storage")

;; Message types
(defconst +wave-server-msg-types+
  '((test-component . "test-component")
    (load-file . "load-file")
    (execute-function . "execute-function")
    (get-status . "get-status")
    (ping . "ping")))

;; Server functions
(defun wave-server-start ()
  "Start the wave function server"
  (interactive)
  (when wave-server-running
    (message "Server is already running")
    (return))
  
  (message "Starting wave function server on port %d..." wave-server-port)
  
  ;; Initialize server state
  (setq wave-server-state (make-hash-table :test 'equal))
  (puthash "status" "running" wave-server-state)
  (puthash "start-time" (current-time-string) wave-server-state)
  (puthash "components-loaded" nil wave-server-state)
  
  ;; Load core components
  (wave-server-load-core-components)
  
  (setq wave-server-running t)
  (message "✓ Wave function server started successfully")
  (message "Server status: %s" (gethash "status" wave-server-state)))

(defun wave-server-stop ()
  "Stop the wave function server"
  (interactive)
  (when (not wave-server-running)
    (message "Server is not running")
    (return))
  
  (message "Stopping wave function server...")
  (setq wave-server-running nil)
  (setq wave-server-clients nil)
  (message "✓ Wave function server stopped"))

(defun wave-server-load-core-components ()
  "Load core wave function components"
  (message "Loading core components...")
  
  (let ((components '("wave-function-core.el"
                      "wave-geometric-solids.el"
                      "wave-archimedean.el"
                      "wave-function-engine.el")))
    (dolist (component components)
      (condition-case err
          (progn
            (load-file (expand-file-name component (file-name-directory load-file-name)))
            (message "✓ Loaded %s" component)
            (push component (gethash "components-loaded" wave-server-state)))
        (error
         (message "✗ Failed to load %s: %s" component err))))))

(defun wave-server-handle-message (message)
  "Handle incoming message from client"
  (let ((msg-type (cdr (assoc 'type message)))
        (msg-data (cdr (assoc 'data message))))
    (cond
     ((string= msg-type "ping")
      (wave-server-send-response "pong" "Server is alive"))
     
     ((string= msg-type "get-status")
      (wave-server-send-status))
     
     ((string= msg-type "test-component")
      (wave-server-test-component msg-data))
     
     ((string= msg-type "load-file")
      (wave-server-load-file msg-data))
     
     ((string= msg-type "execute-function")
      (wave-server-execute-function msg-data))
     
     (t
      (wave-server-send-response "error" (format "Unknown message type: %s" msg-type))))))

(defun wave-server-send-response (type data)
  "Send response to client"
  (let ((response (list (cons 'type type)
                        (cons 'data data)
                        (cons 'timestamp (current-time-string)))))
    (message "Server response: %s" (json-encode response))
    response))

(defun wave-server-send-status ()
  "Send server status"
  (let ((status (list (cons 'running wave-server-running)
                      (cons 'port wave-server-port)
                      (cons 'clients (length wave-server-clients))
                      (cons 'components (gethash "components-loaded" wave-server-state))
                      (cons 'uptime (current-time-string)))))
    (wave-server-send-response "status" status)))

(defun wave-server-test-component (component-name)
  "Test a specific component"
  (let ((filename (format "%s.el" component-name)))
    (condition-case err
        (progn
          (load-file (expand-file-name filename (file-name-directory load-file-name)))
          (wave-server-send-response "success" (format "Component %s loaded successfully" component-name)))
      (error
       (wave-server-send-response "error" (format "Failed to load %s: %s" component-name err))))))

(defun wave-server-load-file (filename)
  "Load a specific file"
  (condition-case err
      (progn
        (load-file filename)
        (wave-server-send-response "success" (format "File %s loaded successfully" filename)))
    (error
     (wave-server-send-response "error" (format "Failed to load %s: %s" filename err)))))

(defun wave-server-execute-function (function-call)
  "Execute a function call"
  (let ((function-name (cdr (assoc 'function function-call)))
        (args (cdr (assoc 'args function-call))))
    (condition-case err
        (progn
          (let ((result (apply (intern function-name) args)))
            (wave-server-send-response "success" (format "Function %s executed: %s" function-name result))))
      (error
       (wave-server-send-response "error" (format "Failed to execute %s: %s" function-name err))))))

;; Interactive commands
(defun wave-server-test-geometric-shapes ()
  "Test geometric shapes functionality"
  (interactive)
  (condition-case err
      (progn
        (let ((tetrahedron (wave-function-create-platonic-solid 'tetrahedron)))
          (message "✓ Tetrahedron created: %s" (geometric-shape-name tetrahedron))
          (wave-server-send-response "success" (format "Tetrahedron created: %s" (geometric-shape-name tetrahedron)))))
    (error
     (message "✗ Geometric shapes error: %s" err)
     (wave-server-send-response "error" (format "Geometric shapes error: %s" err)))))

(defun wave-server-test-church-encoding ()
  "Test Church encoding functionality"
  (interactive)
  (condition-case err
      (progn
        (let ((zero (church-zero 'identity 0))
              (one (church-one 'identity 0)))
          (message "✓ Church encoding working: zero=%s, one=%s" zero one)
          (wave-server-send-response "success" (format "Church encoding working: zero=%s, one=%s" zero one))))
    (error
     (message "✗ Church encoding error: %s" err)
     (wave-server-send-response "error" (format "Church encoding error: %s" err)))))

(defun wave-server-test-wave-creation ()
  "Test wave function creation"
  (interactive)
  (condition-case err
      (progn
        (let ((wave (create-wave-function-church "test-wave" 440.0 0.8 0.0 nil)))
          (message "✓ Wave function created: %s" (identity-wave-function-id wave))
          (wave-server-send-response "success" (format "Wave function created: %s" (identity-wave-function-id wave)))))
    (error
     (message "✗ Wave function creation error: %s" err)
     (wave-server-send-response "error" (format "Wave function creation error: %s" err)))))

;; Server main loop (simplified for testing)
(defun wave-server-main-loop ()
  "Main server loop for testing"
  (interactive)
  (message "=== Wave Function Server Main Loop ===")
  (message "Available commands:")
  (message "  M-x wave-server-start")
  (message "  M-x wave-server-stop")
  (message "  M-x wave-server-test-geometric-shapes")
  (message "  M-x wave-server-test-church-encoding")
  (message "  M-x wave-server-test-wave-creation")
  (message "  M-x wave-server-send-status")
  (message "")
  (message "Server status: %s" (if wave-server-running "Running" "Stopped"))
  (message "Port: %d" wave-server-port))

;; Initialize server
(wave-server-main-loop)

(provide 'wave-server)
