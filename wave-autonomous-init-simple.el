;;; wave-autonomous-init-simple.el --- Simplified autonomous system initialization

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: autonomous, initialization, wave-function

;;; Commentary:
;; Simplified autonomous system initialization and quick start menu

;;; Code:

(require 'cl-lib)
(require 'wave-autonomous-repl-simple)

;;; System Status

(defvar wave-autonomous-system-status 'stopped
  "Current status of the autonomous system.")

(defvar wave-autonomous-modules-loaded nil
  "List of loaded autonomous modules.")

;;; Module Loading

(defun wave-autonomous-load-module (module-name)
  "Load an autonomous module."
  (let ((module-file (format "wave-%s.el" module-name)))
    (if (file-exists-p module-file)
        (progn
          (load-file module-file)
          (add-to-list 'wave-autonomous-modules-loaded module-name)
          (message "✅ Loaded module: %s" module-name)
          t)
      (message "❌ Module not found: %s" module-file)
      nil)))

(defun wave-autonomous-load-core-modules ()
  "Load core autonomous modules."
  (let ((core-modules '("y-combinator" "async-framework" "workflow-engine")))
    (dolist (module core-modules)
      (wave-autonomous-load-module module))
    (message "✅ Core modules loaded: %s" wave-autonomous-modules-loaded)))

;;; System Initialization

(defun wave-autonomous-system-initialize ()
  "Initialize the autonomous system."
  (message "🚀 Initializing Wave Autonomous System...")
  
  ;; Load core modules
  (wave-autonomous-load-core-modules)
  
  ;; Set system status
  (setq wave-autonomous-system-status 'initialized)
  
  (message "✅ Wave Autonomous System initialized successfully")
  (message "📊 Loaded modules: %s" wave-autonomous-modules-loaded))

(defun wave-autonomous-system-status-check ()
  "Check the status of the autonomous system."
  (message "📊 Autonomous System Status:")
  (message "   Status: %s" wave-autonomous-system-status)
  (message "   Loaded modules: %s" wave-autonomous-modules-loaded)
  (message "   Module count: %d" (length wave-autonomous-modules-loaded)))

;;; Quick Start Menu

(defun wave-autonomous-quick-start-menu ()
  "Display the quick start menu."
  (interactive)
  (let ((choice (completing-read
                 "Wave Autonomous System - Quick Start: "
                 '("Initialize System"
                   "Start REPL (Full Supervision)"
                   "Start REPL (Guided Autonomy)"
                   "Start REPL (Full Autonomy)"
                   "System Status"
                   "Test Core Functionality"
                   "Exit"))))
    (pcase choice
      ("Initialize System"
       (wave-autonomous-system-initialize))
      ("Start REPL (Full Supervision)"
       (wave-autonomous-repl-simple-start 'full-supervision))
      ("Start REPL (Guided Autonomy)"
       (wave-autonomous-repl-simple-start 'guided-autonomy))
      ("Start REPL (Full Autonomy)"
       (wave-autonomous-repl-simple-start 'full-autonomy))
      ("System Status"
       (wave-autonomous-system-status-check))
      ("Test Core Functionality"
       (wave-autonomous-test-core-functionality))
      ("Exit"
       (message "👋 Goodbye!")))))

;;; Core Functionality Test

(defun wave-autonomous-test-core-functionality ()
  "Test core autonomous functionality."
  (interactive)
  (message "🧪 Testing Core Autonomous Functionality")
  (message "=====================================")
  
  ;; Test 1: System initialization
  (wave-autonomous-system-initialize)
  
  ;; Test 2: REPL functionality
  (wave-autonomous-repl-simple-test)
  
  ;; Test 3: Status check
  (wave-autonomous-system-status-check)
  
  (message "🎉 Core functionality test completed!"))

;;; Main Entry Point

(defun wave-autonomous-start ()
  "Start the Wave Autonomous System."
  (interactive)
  (message "🌟 Welcome to Wave Autonomous System!")
  (wave-autonomous-quick-start-menu))

;;; Auto-initialization

(defun wave-autonomous-auto-init ()
  "Auto-initialize the system on load."
  (wave-autonomous-system-initialize))

;; Auto-initialize when loaded
(wave-autonomous-auto-init)

(provide 'wave-autonomous-init-simple)

;;; wave-autonomous-init-simple.el ends here
