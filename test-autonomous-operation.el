;;; test-autonomous-operation.el --- Comprehensive tests for autonomous operation

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: test, autonomous, operation, wave-function
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-y-combinator "1.0") (wave-async-framework "1.0") (wave-workflow-engine "1.0") (wave-incidence-workflow "1.0") (wave-protocol-adapter "1.0") (wave-autonomous-repl "1.0") (wave-autonomous-init "1.0"))

;;; Commentary:
;; Comprehensive test suite for autonomous operation system:
;; - Y-combinator levels (private, public, shared)
;; - Async/await/try/catch workflow
;; - REPL modes (full-supervision, guided-autonomy, full-autonomy)
;; - YAML workflow loading and execution
;; - Protocol adapters (emacsclient, websocket, mqtt)
;; - Incidence-based workflows
;; - System integration tests

;;; Code:

(require 'cl-lib)
(require 'wave-y-combinator)
(require 'wave-async-framework)
(require 'wave-workflow-engine)
(require 'wave-incidence-workflow)
(require 'wave-protocol-adapter)
(require 'wave-autonomous-repl)
(require 'wave-autonomous-init)

;;; Test Framework

(defvar wave-test-results (make-hash-table :test 'equal)
  "Test results storage")

(defun wave-test-run (test-name test-func)
  "Run a test and store results"
  (let ((start-time (float-time)))
    (condition-case err
        (progn
          (funcall test-func)
          (puthash test-name (list :status 'passed :time (- (float-time) start-time)) wave-test-results)
          (message "✅ %s: PASSED (%.3fs)" test-name (- (float-time) start-time)))
      (error
       (puthash test-name (list :status 'failed :error (error-message-string err) :time (- (float-time) start-time)) wave-test-results)
       (message "❌ %s: FAILED - %s (%.3fs)" test-name (error-message-string err) (- (float-time) start-time))))))

(defun wave-test-summary ()
  "Display test summary"
  (let ((passed 0)
        (failed 0)
        (total-time 0.0))
    (maphash (lambda (test-name result)
               (let ((status (plist-get result :status))
                     (time (plist-get result :time)))
                 (if (eq status 'passed)
                     (setq passed (1+ passed))
                   (setq failed (1+ failed)))
                 (setq total-time (+ total-time time))))
             wave-test-results)
    
    (message "\n📊 Test Summary:")
    (message "  Total Tests: %d" (+ passed failed))
    (message "  Passed: %d" passed)
    (message "  Failed: %d" failed)
    (message "  Total Time: %.3fs" total-time)
    (message "  Success Rate: %.1f%%" (* 100.0 (/ passed (+ passed failed))))))

;;; Y-Combinator Tests

(defun test-y-combinator-levels ()
  "Test private, public, and shared Y-combinators"
  (message "Testing Y-combinator levels...")
  
  ;; Test private Y-combinator
  (let ((fact-5 (wave-y-combinator-factorial 5)))
    (unless (= fact-5 120)
      (error "Private Y-combinator factorial test failed: expected 120, got %d" fact-5)))
  
  ;; Test public Y-combinator
  (let ((fact-5-public (wave-y-combinator-factorial-with-context 5 'public)))
    (unless (= fact-5-public 120)
      (error "Public Y-combinator factorial test failed: expected 120, got %d" fact-5-public)))
  
  ;; Test shared Y-combinator
  (let ((fact-5-shared (wave-y-combinator-factorial-with-context 5 'shared)))
    (unless (= fact-5-shared 120)
      (error "Shared Y-combinator factorial test failed: expected 120, got %d" fact-5-shared)))
  
  ;; Test Fibonacci
  (let ((fib-10 (wave-y-combinator-fibonacci 10)))
    (unless (= fib-10 55)
      (error "Y-combinator fibonacci test failed: expected 55, got %d" fib-10))))

(defun test-y-combinator-context-management ()
  "Test Y-combinator context management"
  (let ((context (wave-y-combinator-context-create 'public)))
    (unless (wave-y-combinator-context-p context)
      (error "Context creation failed"))
    
    (unless (eq (wave-y-combinator-context-level context) 'public)
      (error "Context level not set correctly"))
    
    (unless (wave-y-combinator-context-approval-required context)
      (error "Public context should require approval"))))

;;; Async Framework Tests

(defun test-async-workflow ()
  "Test async/await/try/catch workflow"
  (message "Testing async framework...")
  
  ;; Test timer-based async execution
  (let ((operation (wave-async-operation-create
                    "test-timer"
                    (lambda () (sleep-for 0.1) "timer result")
                    'timer)))
    (wave-async-execute operation)
    (let ((result (wave-async-await operation 2.0)))
      (unless (string= result "timer result")
        (error "Timer async test failed: expected 'timer result', got '%s'" result))))
  
  ;; Test try/catch error handling
  (let ((try-op (lambda () (error "Test error")))
        (catch-op (lambda (error op) (message "Caught error: %s" error))))
    (let ((operation (wave-async-try-catch try-op catch-op)))
      (wave-async-await operation 2.0)))
  
  ;; Test async pool
  (let ((pool (wave-async-pool-create "test-pool" 2)))
    (dotimes (i 3)
      (let ((operation (wave-async-operation-create
                        (format "pool-test-%d" i)
                        (lambda () (sleep-for 0.1) (format "Result %d" i))
                        'timer)))
        (wave-async-pool-add-operation pool operation)))
    
    (let ((stats (wave-async-pool-wait-all pool 5.0)))
      (unless (>= (plist-get stats :completed) 2)
        (error "Async pool test failed: expected at least 2 completed, got %d" 
               (plist-get stats :completed))))))

(defun test-async-y-combinator-integration ()
  "Test async framework integration with Y-combinator"
  (let ((y-func (lambda (ctx) (lambda () "async y-combinator result"))))
    (let ((operation (wave-async-y-combinator-execute y-func nil 'timer)))
      (let ((result (wave-async-await operation 2.0)))
        (unless (string= result "async y-combinator result")
          (error "Async Y-combinator integration test failed"))))))

;;; Workflow Engine Tests

(defun test-yaml-workflows ()
  "Test YAML workflow loading and execution"
  (message "Testing YAML workflow system...")
  
  ;; Test YAML parsing
  (let ((test-yaml "workflow:
  id: \"test-workflow\"
  type: \"repl\"
  y_combinator_level: \"public\"
  
  read:
    source: \"user-input\"
    input_type: \"s-expression\"
    
  eval:
    transform: \"wave-workflow-eval-default\"
    
  print:
    output: \"buffer\"
    target: \"*Test-Output*\"
    
  loop:
    condition: \"(lambda (ctx) (not (plist-get ctx :quit)))\"
    
  async:
    enabled: true
    executor: \"hybrid\"
    timeout: 5.0"))
    
    (let ((workflow (wave-workflow-parse-yaml test-yaml)))
      (unless (wave-workflow-p workflow)
        (error "YAML parsing failed"))
      
      (unless (string= (wave-workflow-workflow-id workflow) "test-workflow")
        (error "Workflow ID not parsed correctly"))
      
      (unless (eq (wave-workflow-y-combinator-level workflow) 'public)
        (error "Y-combinator level not parsed correctly"))
      
      (unless (wave-workflow-async-enabled workflow)
        (error "Async enabled not parsed correctly"))))
  
  ;; Test workflow execution
  (let ((workflow (wave-workflow-create-repl 'guided-autonomy 'shared)))
    (unless (wave-workflow-p workflow)
      (error "Workflow creation failed"))
    
    (wave-workflow-register workflow)
    (unless (wave-workflow-get (wave-workflow-workflow-id workflow))
      (error "Workflow registration failed"))))

;;; Incidence Workflow Tests

(defun test-incidence-workflows ()
  "Test incidence-based workflow system"
  (message "Testing incidence workflow system...")
  
  ;; Test YAML parsing for incidence workflows
  (let ((test-yaml "incidence_workflow:
  id: \"test-incidence-workflow\"
  
  vertices:
    - id: \"v0-read-input\"
      operation: \"read-user-input\"
      type: \"input\"
      
    - id: \"v1-analyze\"
      operation: \"autonomous-analyze-pattern\"
      type: \"eval\"
      
  edges:
    - id: \"e0\"
      from: \"v0-read-input\"
      to: \"v1-analyze\"
      condition: \"input-valid\"
      
  incidence_matrix:
    rows: [\"v0\", \"v1\"]
    cols: [\"e0\"]
    data: [[1], [0]]
    
  spo_modality:
    subject: \"autonomous-agent\"
    predicate: \"learns-from\"
    object: \"user-input\"
    modality: \"supervised\""))
    
    (let ((workflow (wave-incidence-workflow-parse-yaml test-yaml)))
      (unless (wave-incidence-workflow-p workflow)
        (error "Incidence workflow parsing failed"))
      
      (unless (= (length (wave-incidence-workflow-vertices workflow)) 2)
        (error "Vertices not parsed correctly"))
      
      (unless (= (length (wave-incidence-workflow-edges workflow)) 1)
        (error "Edges not parsed correctly"))
      
      (unless (wave-incidence-matrix-validate workflow)
        (error "Incidence matrix validation failed"))))
  
  ;; Test incidence matrix operations
  (let ((workflow (wave-incidence-workflow-from-yaml "test-incidence-workflow.yaml")))
    (when workflow
      (let ((matrix-value (wave-incidence-matrix-get workflow "v0" "e0")))
        (unless (= matrix-value 1)
          (error "Incidence matrix get failed: expected 1, got %s" matrix-value))))))

;;; Protocol Adapter Tests

(defun test-protocol-adapters ()
  "Test emacsclient, websocket, MQTT protocols"
  (message "Testing protocol adapters...")
  
  ;; Test Emacs client protocol
  (let ((adapter (wave-protocol-emacsclient-create-adapter "test-emacsclient")))
    (unless (wave-protocol-adapter-p adapter)
      (error "Emacs client adapter creation failed"))
    
    (unless (eq (wave-protocol-adapter-protocol-type adapter) 'emacsclient)
      (error "Protocol type not set correctly"))
    
    (wave-protocol-adapter-register adapter)
    (unless (wave-protocol-adapter-get "test-emacsclient")
      (error "Adapter registration failed")))
  
  ;; Test WebSocket protocol (if available)
  (when (require 'websocket nil t)
    (let ((adapter (wave-protocol-websocket-create-adapter "test-websocket")))
      (unless (wave-protocol-adapter-p adapter)
        (error "WebSocket adapter creation failed"))))
  
  ;; Test MQTT protocol (if available)
  (when (require 'mqtt nil t)
    (let ((adapter (wave-protocol-mqtt-create-adapter "test-mqtt")))
      (unless (wave-protocol-adapter-p adapter)
        (error "MQTT adapter creation failed"))))
  
  ;; Test message creation and sending
  (let ((adapter (wave-protocol-emacsclient-create-adapter "test-message")))
    (let ((message (wave-protocol-message-create "test message" "test" "wave-function" 'text)))
      (unless (wave-protocol-message-p message)
        (error "Message creation failed"))
      
      (unless (string= (wave-protocol-message-content message) "test message")
        (error "Message content not set correctly")))))

;;; REPL Tests

(defun test-repl-modes ()
  "Test REPL at all three control levels"
  (message "Testing REPL modes...")
  
  ;; Test full supervision mode
  (let ((repl1 (wave-autonomous-repl-start "full-supervision")))
    (unless (wave-autonomous-repl-p repl1)
      (error "Full supervision REPL creation failed"))
    
    (unless (eq (wave-autonomous-repl-control-level repl1) 'full-supervision)
      (error "Control level not set correctly"))
    
    (unless (eq (wave-autonomous-repl-y-combinator-level repl1) 'public)
      (error "Y-combinator level not set correctly"))
    
    (wave-autonomous-repl-stop repl1))
  
  ;; Test guided autonomy mode
  (let ((repl2 (wave-autonomous-repl-start "guided-autonomy")))
    (unless (eq (wave-autonomous-repl-control-level repl2) 'guided-autonomy)
      (error "Guided autonomy control level not set correctly"))
    
    (unless (eq (wave-autonomous-repl-y-combinator-level repl2) 'shared)
      (error "Guided autonomy Y-combinator level not set correctly"))
    
    (wave-autonomous-repl-stop repl2))
  
  ;; Test full autonomy mode
  (let ((repl3 (wave-autonomous-repl-start "full-autonomy")))
    (unless (eq (wave-autonomous-repl-control-level repl3) 'full-autonomy)
      (error "Full autonomy control level not set correctly"))
    
    (unless (eq (wave-autonomous-repl-y-combinator-level repl3) 'private)
      (error "Full autonomy Y-combinator level not set correctly"))
    
    (wave-autonomous-repl-stop repl3)))

(defun test-repl-approval-system ()
  "Test REPL approval system"
  (let ((repl (wave-autonomous-repl-start "full-supervision")))
    (let ((input (make-wave-repl-input
                  :input-id "test-input"
                  :content "dangerous-operation"
                  :input-type 's-expression
                  :timestamp (float-time)
                  :source 'user)))
      
      (unless (wave-repl-needs-approval repl input)
        (error "Dangerous operation should need approval"))
      
      (wave-repl-queue-for-approval repl input)
      (unless (= (length (wave-autonomous-repl-approval-queue repl)) 1)
        (error "Input not queued for approval"))
      
      (wave-repl-approve-input repl input)
      (unless (= (length (wave-autonomous-repl-approval-queue repl)) 0)
        (error "Input not removed from approval queue after approval")))
    
    (wave-autonomous-repl-stop repl)))

;;; System Integration Tests

(defun test-system-integration ()
  "Test full system integration"
  (message "Testing system integration...")
  
  ;; Initialize system
  (wave-autonomous-system-init)
  
  ;; Check system health
  (let ((health (wave-autonomous-system-health)))
    (unless (eq (plist-get health :overall) 'healthy)
      (error "System health check failed: %s" (plist-get health :issues))))
  
  ;; Test workflow loading
  (let ((workflows (wave-workflow-load-directory "~/.emacs.d/wave-function/workflows/")))
    (unless (> (length workflows) 0)
      (error "No workflows loaded from directory")))
  
  ;; Cleanup
  (wave-autonomous-system-cleanup))

(defun test-workflow-execution ()
  "Test workflow execution with different types"
  (message "Testing workflow execution...")
  
  ;; Test standard workflow
  (let ((workflow (wave-workflow-create-repl 'guided-autonomy 'shared)))
    (wave-workflow-execute workflow))
  
  ;; Test incidence workflow
  (let ((workflow (wave-incidence-workflow-from-yaml "test-incidence-workflow.yaml")))
    (when workflow
      (wave-incidence-workflow-execute workflow))))

;;; Main Test Runner

(defun test-autonomous-operation-all ()
  "Run all autonomous operation tests"
  (interactive)
  (message "🧪 Starting comprehensive autonomous operation tests...")
  
  ;; Clear previous results
  (clrhash wave-test-results)
  
  ;; Y-combinator tests
  (wave-test-run "Y-combinator levels" 'test-y-combinator-levels)
  (wave-test-run "Y-combinator context management" 'test-y-combinator-context-management)
  
  ;; Async framework tests
  (wave-test-run "Async workflow" 'test-async-workflow)
  (wave-test-run "Async Y-combinator integration" 'test-async-y-combinator-integration)
  
  ;; Workflow engine tests
  (wave-test-run "YAML workflows" 'test-yaml-workflows)
  
  ;; Incidence workflow tests
  (wave-test-run "Incidence workflows" 'test-incidence-workflows)
  
  ;; Protocol adapter tests
  (wave-test-run "Protocol adapters" 'test-protocol-adapters)
  
  ;; REPL tests
  (wave-test-run "REPL modes" 'test-repl-modes)
  (wave-test-run "REPL approval system" 'test-repl-approval-system)
  
  ;; System integration tests
  (wave-test-run "System integration" 'test-system-integration)
  (wave-test-run "Workflow execution" 'test-workflow-execution)
  
  ;; Display summary
  (wave-test-summary)
  
  (message "🎉 All autonomous operation tests completed!"))

(defun test-autonomous-operation-quick ()
  "Run quick subset of tests"
  (interactive)
  (message "🧪 Running quick autonomous operation tests...")
  
  (clrhash wave-test-results)
  
  (wave-test-run "Y-combinator levels" 'test-y-combinator-levels)
  (wave-test-run "Async workflow" 'test-async-workflow)
  (wave-test-run "REPL modes" 'test-repl-modes)
  
  (wave-test-summary))

;;; Individual Test Commands

(defun test-y-combinator-levels-command ()
  "Test Y-combinator levels command"
  (interactive)
  (wave-test-run "Y-combinator levels" 'test-y-combinator-levels))

(defun test-async-workflow-command ()
  "Test async workflow command"
  (interactive)
  (wave-test-run "Async workflow" 'test-async-workflow))

(defun test-repl-modes-command ()
  "Test REPL modes command"
  (interactive)
  (wave-test-run "REPL modes" 'test-repl-modes))

(defun test-yaml-workflows-command ()
  "Test YAML workflows command"
  (interactive)
  (wave-test-run "YAML workflows" 'test-yaml-workflows))

(defun test-protocol-adapters-command ()
  "Test protocol adapters command"
  (interactive)
  (wave-test-run "Protocol adapters" 'test-protocol-adapters))

;;; Global Keybindings

(global-set-key (kbd "C-c t a") 'test-autonomous-operation-all)
(global-set-key (kbd "C-c t q") 'test-autonomous-operation-quick)
(global-set-key (kbd "C-c t y") 'test-y-combinator-levels-command)
(global-set-key (kbd "C-c t w") 'test-async-workflow-command)
(global-set-key (kbd "C-c t r") 'test-repl-modes-command)
(global-set-key (kbd "C-c t f") 'test-yaml-workflows-command)
(global-set-key (kbd "C-c t p") 'test-protocol-adapters-command)

(provide 'test-autonomous-operation)

;;; test-autonomous-operation.el ends here
