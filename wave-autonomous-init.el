;;; wave-autonomous-init.el --- System initialization for autonomous operation

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: initialization, autonomous, system, wave-function
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-y-combinator "1.0") (wave-async-framework "1.0") (wave-workflow-engine "1.0") (wave-incidence-workflow "1.0") (wave-protocol-adapter "1.0") (wave-autonomous-repl "1.0"))

;;; Commentary:
;; Implements system initialization for full autonomous operation:
;; - Protocol adapter initialization (emacsclient, websocket, mqtt)
;; - Workflow definition loading from YAML files
;; - Autonomous evolution engine startup
;; - Default REPL initialization
;; - Quick start menu for different operation modes
;; - System health monitoring and diagnostics

;;; Code:

(require 'cl-lib)
(require 'wave-y-combinator)
(require 'wave-async-framework)
(require 'wave-workflow-engine)
(require 'wave-incidence-workflow)
(require 'wave-protocol-adapter)
(require 'wave-autonomous-repl)

;;; System Initialization

(defun wave-autonomous-system-init ()
  "Initialize full autonomous operation system"
  (interactive)
  
  (message "🚀 Initializing Wave Function Autonomous System...")
  
  ;; Initialize protocol adapters
  (wave-protocol-emacsclient-init)
  (message "✅ Emacs client protocol initialized")
  
  ;; Load workflow definitions
  (let ((workflow-dir "~/.emacs.d/wave-function/workflows/"))
    (when (file-directory-p workflow-dir)
      (wave-workflow-load-directory workflow-dir)
      (message "✅ Workflow definitions loaded from %s" workflow-dir)))
  
  ;; Start autonomous evolution engine
  (let ((engine (autonomous-evolution-engine-create "main-autonomous-engine" 0.15)))
    (autonomous-evolution-engine-register engine)
    (message "✅ Autonomous evolution engine started"))
  
  ;; Initialize default protocol adapters
  (wave-autonomous-init-protocol-adapters)
  
  ;; Initialize default REPL
  (wave-autonomous-repl-start "guided-autonomy")
  (message "✅ Default autonomous REPL started")
  
  ;; Setup system monitoring
  (wave-autonomous-setup-monitoring)
  
  (message "🎉 Wave Function Autonomous System initialized successfully!")
  (wave-autonomous-system-status))

(defun wave-autonomous-init-protocol-adapters ()
  "Initialize default protocol adapters"
  (let ((adapters nil))
    
    ;; Emacs client adapter
    (let ((emacs-adapter (wave-protocol-emacsclient-create-adapter "default-emacsclient")))
      (wave-protocol-adapter-connect emacs-adapter)
      (push emacs-adapter adapters))
    
    ;; WebSocket adapter (if available)
    (when (require 'websocket nil t)
      (let ((ws-adapter (wave-protocol-websocket-create-adapter "default-websocket")))
        (wave-protocol-adapter-connect ws-adapter)
        (push ws-adapter adapters)))
    
    ;; MQTT adapter (if available)
    (when (require 'mqtt nil t)
      (let ((mqtt-adapter (wave-protocol-mqtt-create-adapter "default-mqtt")))
        (wave-protocol-adapter-connect mqtt-adapter)
        (push mqtt-adapter adapters)))
    
    (message "✅ Protocol adapters initialized: %d adapters" (length adapters))
    adapters))

(defun wave-autonomous-setup-monitoring ()
  "Setup system monitoring and health checks"
  (let ((monitor-timer (run-with-timer 30 30 'wave-autonomous-health-check)))
    (message "✅ System monitoring started (30s intervals)")))

(defun wave-autonomous-health-check ()
  "Perform system health check"
  (let ((health-status (wave-autonomous-system-health)))
    (when (not (eq (plist-get health-status :overall) 'healthy))
      (message "⚠️ System health warning: %s" (plist-get health-status :issues)))))

;;; Quick Start Menu

(defun wave-autonomous-quick-start ()
  "Quick start menu for autonomous operation"
  (interactive)
  (let ((choice (completing-read 
                 "Autonomous Operation Mode: "
                 '("Full Supervision (public Y-combinator)"
                   "Guided Autonomy (shared Y-combinator)"
                   "Full Autonomy (private Y-combinator)"
                   "Custom Workflow from YAML"
                   "System Status & Diagnostics"
                   "Initialize Full System"))))
    (pcase choice
      ("Full Supervision (public Y-combinator)"
       (wave-autonomous-repl-start "full-supervision"))
      ("Guided Autonomy (shared Y-combinator)"
       (wave-autonomous-repl-start "guided-autonomy"))
      ("Full Autonomy (private Y-combinator)"
       (wave-autonomous-repl-start "full-autonomy"))
      ("Custom Workflow from YAML"
       (call-interactively 'wave-workflow-load-and-execute))
      ("System Status & Diagnostics"
       (wave-autonomous-system-status))
      ("Initialize Full System"
       (wave-autonomous-system-init)))))

(defun wave-workflow-load-and-execute ()
  "Load and execute workflow from YAML file"
  (interactive)
  (let ((yaml-file (read-file-name "Load workflow from YAML file: " 
                                   "~/.emacs.d/wave-function/workflows/" 
                                   nil t "*.yaml")))
    (when yaml-file
      (let ((workflow (wave-workflow-load-yaml yaml-file)))
        (if workflow
            (progn
              (wave-workflow-execute workflow)
              (message "✅ Workflow executed: %s" (wave-workflow-workflow-id workflow)))
          (message "❌ Failed to load workflow from: %s" yaml-file))))))

;;; System Status and Diagnostics

(defun wave-autonomous-system-status ()
  "Display comprehensive system status"
  (interactive)
  (let ((status (wave-autonomous-system-health)))
    (with-current-buffer (get-buffer-create "*Wave-System-Status*")
      (erase-buffer)
      (insert "Wave Function Autonomous System Status\n")
      (insert "=====================================\n\n")
      
      ;; Overall status
      (insert (format "Overall Status: %s\n" (plist-get status :overall)))
      (insert (format "Timestamp: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      ;; Y-combinator status
      (insert "Y-Combinator System:\n")
      (insert (format "  Registry: %d contexts\n" (hash-table-count wave-y-combinator-registry)))
      (insert (format "  Status: %s\n\n" (plist-get status :y-combinator)))
      
      ;; Async framework status
      (insert "Async Framework:\n")
      (insert (format "  Operations: %d\n" (hash-table-count wave-async-operation-registry)))
      (insert (format "  Pools: %d\n" (hash-table-count wave-async-pool-registry)))
      (insert (format "  Status: %s\n\n" (plist-get status :async-framework)))
      
      ;; Workflow engine status
      (insert "Workflow Engine:\n")
      (insert (format "  Workflows: %d\n" (length (wave-workflow-list-all))))
      (insert (format "  Status: %s\n\n" (plist-get status :workflow-engine)))
      
      ;; Incidence workflow status
      (insert "Incidence Workflows:\n")
      (insert (format "  Workflows: %d\n" (hash-table-count wave-incidence-workflow-registry)))
      (insert (format "  Status: %s\n\n" (plist-get status :incidence-workflow)))
      
      ;; Protocol adapters status
      (insert "Protocol Adapters:\n")
      (insert (format "  Adapters: %d\n" (length (wave-protocol-adapter-list-all))))
      (insert (format "  Status: %s\n\n" (plist-get status :protocol-adapters)))
      
      ;; Autonomous REPL status
      (insert "Autonomous REPLs:\n")
      (insert (format "  REPLs: %d\n" (length (wave-autonomous-repl-list-all))))
      (insert (format "  Status: %s\n\n" (plist-get status :autonomous-repl)))
      
      ;; Issues
      (when (plist-get status :issues)
        (insert "Issues:\n")
        (dolist (issue (plist-get status :issues))
          (insert (format "  ⚠️ %s\n" issue)))
        (insert "\n"))
      
      (switch-to-buffer-other-window (current-buffer)))))

(defun wave-autonomous-system-health ()
  "Get comprehensive system health status"
  (let ((health-status (list :overall 'healthy :issues nil)))
    
    ;; Check Y-combinator system
    (let ((y-combinator-health (wave-autonomous-check-y-combinator-health)))
      (plist-put health-status :y-combinator y-combinator-health))
    
    ;; Check async framework
    (let ((async-health (wave-autonomous-check-async-health)))
      (plist-put health-status :async-framework async-health))
    
    ;; Check workflow engine
    (let ((workflow-health (wave-autonomous-check-workflow-health)))
      (plist-put health-status :workflow-engine workflow-health))
    
    ;; Check incidence workflow
    (let ((incidence-health (wave-autonomous-check-incidence-health)))
      (plist-put health-status :incidence-workflow incidence-health))
    
    ;; Check protocol adapters
    (let ((protocol-health (wave-autonomous-check-protocol-health)))
      (plist-put health-status :protocol-adapters protocol-health))
    
    ;; Check autonomous REPL
    (let ((repl-health (wave-autonomous-check-repl-health)))
      (plist-put health-status :autonomous-repl repl-health))
    
    ;; Determine overall health
    (let ((all-health (list (plist-get health-status :y-combinator)
                           (plist-get health-status :async-framework)
                           (plist-get health-status :workflow-engine)
                           (plist-get health-status :incidence-workflow)
                           (plist-get health-status :protocol-adapters)
                           (plist-get health-status :autonomous-repl))))
      (when (member 'unhealthy all-health)
        (plist-put health-status :overall 'unhealthy)
        (plist-put health-status :issues 
                   (append (plist-get health-status :issues)
                           (list "One or more subsystems are unhealthy")))))
    
    health-status))

(defun wave-autonomous-check-y-combinator-health ()
  "Check Y-combinator system health"
  (if (> (hash-table-count wave-y-combinator-registry) 0)
      'healthy
    'warning))

(defun wave-autonomous-check-async-health ()
  "Check async framework health"
  (if (and (> (hash-table-count wave-async-operation-registry) 0)
           (> (hash-table-count wave-async-pool-registry) 0))
      'healthy
    'warning))

(defun wave-autonomous-check-workflow-health ()
  "Check workflow engine health"
  (if (> (length (wave-workflow-list-all)) 0)
      'healthy
    'warning))

(defun wave-autonomous-check-incidence-health ()
  "Check incidence workflow health"
  (if (> (hash-table-count wave-incidence-workflow-registry) 0)
      'healthy
    'warning))

(defun wave-autonomous-check-protocol-health ()
  "Check protocol adapters health"
  (let ((adapters (wave-protocol-adapter-list-all)))
    (if (> (length adapters) 0)
        'healthy
      'warning)))

(defun wave-autonomous-check-repl-health ()
  "Check autonomous REPL health"
  (let ((repls (wave-autonomous-repl-list-all)))
    (if (> (length repls) 0)
        'healthy
      'warning)))

;;; Workflow Directory Management

(defun wave-autonomous-setup-workflow-directory ()
  "Setup default workflow directory with example workflows"
  (interactive)
  (let ((workflow-dir "~/.emacs.d/wave-function/workflows/"))
    (unless (file-directory-p workflow-dir)
      (make-directory workflow-dir t))
    
    ;; Create example workflows
    (wave-autonomous-create-example-workflows workflow-dir)
    
    (message "✅ Workflow directory setup complete: %s" workflow-dir)))

(defun wave-autonomous-create-example-workflows (workflow-dir)
  "Create example workflow files"
  (let ((examples (list
                   (cons "autonomous-repl.yaml" 
                         "workflow:
  id: \"autonomous-repl-example\"
  type: \"repl\"
  y_combinator_level: \"guided-autonomy\"
  
  read:
    source: \"user-input\"
    input_type: \"s-expression\"
    protocol: \"emacsclient\"
    
  eval:
    transform: \"autonomous-process-input\"
    async:
      enabled: true
      executor: \"hybrid\"
      timeout: 5.0
      
  print:
    output: \"buffer\"
    target: \"*Autonomous-REPL*\"
    format: \"pretty-print\"
    
  loop:
    condition: \"(lambda (ctx) (not (plist-get ctx :quit)))\"
    
  error:
    try: \"eval-transform\"
    catch: \"(lambda (err) (message \\\"Error: %s\\\" err))\"")
                   
                   (cons "incidence-learning.yaml"
                         "incidence_workflow:
  id: \"autonomous-learning-workflow\"
  
  vertices:
    - id: \"v0-read-input\"
      operation: \"read-user-input\"
      type: \"input\"
      
    - id: \"v1-analyze\"
      operation: \"autonomous-analyze-pattern\"
      type: \"eval\"
      
    - id: \"v2-learn\"
      operation: \"autonomous-learn-from-pattern\"
      type: \"eval\"
      
    - id: \"v3-display\"
      operation: \"print-to-buffer\"
      type: \"output\"
      
  edges:
    - id: \"e0\"
      from: \"v0-read-input\"
      to: \"v1-analyze\"
      condition: \"input-valid\"
      
    - id: \"e1\"
      from: \"v1-analyze\"
      to: \"v2-learn\"
      condition: \"pattern-detected\"
      
    - id: \"e2\"
      from: \"v2-learn\"
      to: \"v3-display\"
      condition: \"learning-complete\"
      
  spo_modality:
    subject: \"autonomous-agent\"
    predicate: \"learns-from\"
    object: \"user-input\"
    modality: \"supervised\""))))
    
    (dolist (example examples)
      (let ((file-path (expand-file-name (car example) workflow-dir)))
        (with-temp-file file-path
          (insert (cdr example))))))
    
    (message "✅ Created %d example workflows" (length examples))))

;;; System Cleanup

(defun wave-autonomous-system-cleanup ()
  "Cleanup autonomous system resources"
  (interactive)
  (message "🧹 Cleaning up Wave Function Autonomous System...")
  
  ;; Stop all REPLs
  (dolist (repl (wave-autonomous-repl-list-all))
    (wave-autonomous-repl-stop repl))
  
  ;; Disconnect all protocol adapters
  (dolist (adapter-id (wave-protocol-adapter-list-all))
    (let ((adapter (wave-protocol-adapter-get adapter-id)))
      (when adapter
        (wave-protocol-adapter-disconnect adapter))))
  
  ;; Cancel all async operations
  (maphash (lambda (id operation)
             (wave-async-operation-cancel operation))
           wave-async-operation-registry)
  
  ;; Clear registries
  (clrhash wave-y-combinator-registry)
  (clrhash wave-async-operation-registry)
  (clrhash wave-async-pool-registry)
  (clrhash wave-workflow-registry)
  (clrhash wave-incidence-workflow-registry)
  (clrhash wave-protocol-adapter-registry)
  (clrhash wave-autonomous-repl-registry)
  
  (message "✅ System cleanup completed"))

;;; Global Commands

(defun wave-autonomous-quick-start-menu ()
  "Global quick start menu command"
  (interactive)
  (wave-autonomous-quick-start))

(defun wave-autonomous-system-status-display ()
  "Global system status display command"
  (interactive)
  (wave-autonomous-system-status))

;;; Test Functions

(defun wave-autonomous-test-full-system ()
  "Test full autonomous system initialization"
  (interactive)
  (message "🧪 Testing full autonomous system...")
  
  ;; Initialize system
  (wave-autonomous-system-init)
  
  ;; Wait a moment
  (sit-for 2)
  
  ;; Check status
  (wave-autonomous-system-status)
  
  ;; Cleanup
  (wave-autonomous-system-cleanup)
  
  (message "✅ Full system test completed"))

(defun wave-autonomous-test-workflow-loading ()
  "Test workflow loading functionality"
  (interactive)
  (message "🧪 Testing workflow loading...")
  
  ;; Setup workflow directory
  (wave-autonomous-setup-workflow-directory)
  
  ;; Load workflows
  (let ((workflows (wave-workflow-load-directory "~/.emacs.d/wave-function/workflows/")))
    (message "✅ Loaded %d workflows" (length workflows)))
  
  (message "✅ Workflow loading test completed"))

(defun wave-autonomous-test-all ()
  "Test all autonomous system functionality"
  (interactive)
  (message "🧪 Testing all autonomous system functionality...")
  (wave-autonomous-test-full-system)
  (wave-autonomous-test-workflow-loading)
  (message "✅ All autonomous system tests completed"))

;;; Global Keybindings

(global-set-key (kbd "C-c w s") 'wave-autonomous-quick-start-menu)
(global-set-key (kbd "C-c w t") 'wave-autonomous-system-status-display)
(global-set-key (kbd "C-c w i") 'wave-autonomous-system-init)
(global-set-key (kbd "C-c w c") 'wave-autonomous-system-cleanup)

(provide 'wave-autonomous-init)

;;; wave-autonomous-init.el ends here
