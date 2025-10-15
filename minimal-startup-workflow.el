;;; minimal-startup-workflow.el --- Minimal startup workflow for interactive debugging

;; This provides a minimal startup workflow that loads components step by step
;; and provides interactive debugging capabilities

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load core components first
(message "=== Loading Core Components ===")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-archimedean.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")

(message "✓ Core components loaded successfully")

;; Test basic functionality
(message "=== Testing Basic Functionality ===")

;; Test Church encoding
(condition-case err
    (progn
      (let ((zero (church-zero 'identity 0))
            (one (church-one 'identity 0))
            (two (church-two 'identity 0)))
        (message "✓ Church encoding working: zero=%s, one=%s, two=%s" zero one two)))
  (error
   (message "✗ Church encoding error: %s" err)))

;; Test geometric shapes
(condition-case err
    (progn
      (let ((tetrahedron (wave-function-create-platonic-solid 'tetrahedron)))
        (message "✓ Geometric shapes working: %s" (geometric-shape-name tetrahedron))))
  (error
   (message "✗ Geometric shapes error: %s" err)))

;; Test wave function creation
(condition-case err
    (progn
      (let ((wave (create-wave-function-church "test-wave" 440.0 0.8 0.0 nil)))
        (message "✓ Wave function creation working: %s" (identity-wave-function-id wave))))
  (error
   (message "✗ Wave function creation error: %s" err)))

(message "=== Basic Functionality Test Complete ===")

;; Provide interactive functions
(defun test-individual-component (filename)
  "Test an individual component file"
  (interactive "fComponent file: ")
  (condition-case err
      (progn
        (load-file filename)
        (message "✓ %s loaded successfully" filename))
    (error
     (message "✗ %s failed: %s" filename err))))

(defun test-multiplexer-only ()
  "Test just the multiplexer file"
  (interactive)
  (test-individual-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-multiplexer.el"))

(defun test-identity-management-only ()
  "Test just the identity management file"
  (interactive)
  (test-individual-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-identity-management.el"))

(defun test-autonomous-only ()
  "Test just the autonomous file"
  (interactive)
  (test-individual-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-autonomous.el"))

(defun test-emacs-integration-only ()
  "Test just the emacs integration file"
  (interactive)
  (test-individual-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-emacs-integration.el"))

(message "=== Interactive Functions Available ===")
(message "M-x test-multiplexer-only")
(message "M-x test-identity-management-only")
(message "M-x test-autonomous-only")
(message "M-x test-emacs-integration-only")
(message "M-x test-individual-component")

(provide 'minimal-startup-workflow)
