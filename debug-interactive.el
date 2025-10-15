;;; debug-interactive.el --- Interactive debugging for wave function package

;; This script provides interactive debugging capabilities

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load components one by one with detailed error reporting
(defun debug-load-component (filename)
  "Load a component with detailed error reporting"
  (message "=== Loading %s ===" filename)
  (condition-case err
      (progn
        (load-file filename)
        (message "✓ %s loaded successfully" filename)
        t)
    (error
     (message "✗ %s failed to load: %s" filename err)
     (message "Error details: %s" (error-message-string err))
     nil)))

;; Test each component individually
(defun debug-all-components ()
  "Test all components individually"
  (message "=== Interactive Debug Session ===")
  
  ;; Test core components
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-archimedean.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")
  
  ;; Test multiplexer specifically
  (message "=== Testing Multiplexer ===")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-multiplexer.el")
  
  ;; Test remaining components
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-identity-management.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-epistemic.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-communication.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-autonomous.el")
  (debug-load-component "/home/main/dev/Axiomatic/demos/emacs-demo/wave-emacs-integration.el")
  
  (message "=== Debug Session Complete ==="))

;; Run the debug session
(debug-all-components)

;; Provide the debug functions for interactive use
(provide 'debug-interactive)
