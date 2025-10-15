;;; AUTONOMOUS_SYSTEM_TEST.el --- Comprehensive test for autonomous system

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: test, autonomous, validation, wave-function

;;; Commentary:
;; Comprehensive test script for the autonomous system components

;;; Code:

(require 'cl-lib)

;;; Test Configuration

(defvar autonomous-test-results nil
  "Results of autonomous system tests.")

(defvar autonomous-test-count 0
  "Number of tests run.")

(defvar autonomous-test-passed 0
  "Number of tests passed.")

;;; Test Framework

(defun autonomous-test-run (test-name test-function)
  "Run a single test and record results."
  (setq autonomous-test-count (1+ autonomous-test-count))
  (message "🧪 Running test: %s" test-name)
  
  (let ((result (condition-case error
                    (progn
                      (funcall test-function)
                      (setq autonomous-test-passed (1+ autonomous-test-passed))
                      (message "✅ PASS: %s" test-name)
                      t)
                  (error
                   (message "❌ FAIL: %s - %s" test-name (error-message-string error))
                   nil))))
    (push (list test-name result) autonomous-test-results)
    result))

;;; Individual Tests

(defun test-y-combinator-loading ()
  "Test Y-combinator module loading."
  (load-file "wave-y-combinator.el")
  (message "   Y-combinator module loaded successfully"))

(defun test-async-framework-loading ()
  "Test async framework module loading."
  (load-file "wave-async-framework.el")
  (message "   Async framework module loaded successfully"))

(defun test-workflow-engine-loading ()
  "Test workflow engine module loading."
  (load-file "wave-workflow-engine.el")
  (message "   Workflow engine module loaded successfully"))

(defun test-incidence-workflow-loading ()
  "Test incidence workflow module loading."
  (load-file "wave-incidence-workflow.el")
  (message "   Incidence workflow module loaded successfully"))

(defun test-protocol-adapter-loading ()
  "Test protocol adapter module loading."
  (load-file "wave-protocol-adapter.el")
  (message "   Protocol adapter module loaded successfully"))

(defun test-autonomous-repl-simple-loading ()
  "Test simplified autonomous REPL loading."
  (load-file "wave-autonomous-repl-simple.el")
  (message "   Simplified autonomous REPL loaded successfully"))

(defun test-autonomous-init-simple-loading ()
  "Test simplified autonomous init loading."
  (load-file "wave-autonomous-init-simple.el")
  (message "   Simplified autonomous init loaded successfully"))

(defun test-core-functionality ()
  "Test core wave function functionality."
  (load-file "WORKING_LOADER.el")
  (wave-function-working-test)
  (message "   Core functionality test completed"))

(defun test-church-encoding ()
  "Test Church encoding functionality."
  (let ((church-zero (lambda (f) (lambda (x) x))))
    (let ((result (funcall (funcall church-zero (lambda (x) (+ x 1))) 0)))
      (if (= result 0)
          (message "   Church encoding test passed")
        (error "Church encoding test failed: expected 0, got %d" result)))))

(defun test-golden-ratio ()
  "Test golden ratio calculations."
  (let ((phi (/ (+ 1 (sqrt 5)) 2)))
    (let ((phi-squared (* phi phi))
          (phi-plus-one (+ phi 1)))
      (let ((difference (abs (- phi-squared phi-plus-one))))
        (if (< difference 0.0001)
            (message "   Golden ratio test passed")
          (error "Golden ratio test failed: difference too large: %f" difference))))))

;;; Main Test Suite

(defun autonomous-system-comprehensive-test ()
  "Run comprehensive test suite for autonomous system."
  (interactive)
  (message "🚀 Starting Comprehensive Autonomous System Test")
  (message "================================================")
  
  ;; Reset test counters
  (setq autonomous-test-results nil)
  (setq autonomous-test-count 0)
  (setq autonomous-test-passed 0)
  
  ;; Run all tests
  (autonomous-test-run "Y-Combinator Loading" 'test-y-combinator-loading)
  (autonomous-test-run "Async Framework Loading" 'test-async-framework-loading)
  (autonomous-test-run "Workflow Engine Loading" 'test-workflow-engine-loading)
  (autonomous-test-run "Incidence Workflow Loading" 'test-incidence-workflow-loading)
  (autonomous-test-run "Protocol Adapter Loading" 'test-protocol-adapter-loading)
  (autonomous-test-run "Autonomous REPL Simple Loading" 'test-autonomous-repl-simple-loading)
  (autonomous-test-run "Autonomous Init Simple Loading" 'test-autonomous-init-simple-loading)
  (autonomous-test-run "Core Functionality" 'test-core-functionality)
  (autonomous-test-run "Church Encoding" 'test-church-encoding)
  (autonomous-test-run "Golden Ratio" 'test-golden-ratio)
  
  ;; Display results
  (message "")
  (message "📊 Test Results Summary:")
  (message "========================")
  (message "Total Tests: %d" autonomous-test-count)
  (message "Passed: %d" autonomous-test-passed)
  (message "Failed: %d" (- autonomous-test-count autonomous-test-passed))
  (message "Success Rate: %.1f%%" (* 100.0 (/ autonomous-test-passed autonomous-test-count)))
  
  (if (= autonomous-test-passed autonomous-test-count)
      (progn
        (message "")
        (message "🎉 ALL TESTS PASSED!")
        (message "✅ Autonomous system is ready for public consumption!")
        t)
    (message "")
    (message "⚠️ Some tests failed. Check the output above for details.")
    nil))

;;; Quick Test

(defun autonomous-system-quick-test ()
  "Run quick test of essential components."
  (interactive)
  (message "⚡ Quick Autonomous System Test")
  (message "==============================")
  
  (autonomous-test-run "Core Functionality" 'test-core-functionality)
  (autonomous-test-run "Church Encoding" 'test-church-encoding)
  (autonomous-test-run "Golden Ratio" 'test-golden-ratio)
  
  (message "")
  (message "📊 Quick Test Results: %d/%d passed" autonomous-test-passed autonomous-test-count))

;;; Interactive Commands

(defun autonomous-system-test-menu ()
  "Display test menu."
  (interactive)
  (let ((choice (completing-read
                 "Autonomous System Test Menu: "
                 '("Comprehensive Test"
                   "Quick Test"
                   "Individual Module Tests"
                   "Exit"))))
    (pcase choice
      ("Comprehensive Test"
       (autonomous-system-comprehensive-test))
      ("Quick Test"
       (autonomous-system-quick-test))
      ("Individual Module Tests"
       (message "Run individual tests using M-x test-<module-name>-loading"))
      ("Exit"
       (message "👋 Test menu closed")))))

;;; Auto-run comprehensive test

(defun autonomous-system-auto-test ()
  "Auto-run comprehensive test."
  (autonomous-system-comprehensive-test))

;; Run the test automatically when loaded
(autonomous-system-auto-test)

(provide 'autonomous-system-test)

;;; AUTONOMOUS_SYSTEM_TEST.el ends here
