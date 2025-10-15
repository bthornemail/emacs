;;; test-multiplexer-with-deps.el --- Test multiplexer with dependencies

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load dependencies first
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")

;; Now test the multiplexer file
(condition-case err
    (progn
      (load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-multiplexer.el")
      (message "✓ Multiplexer loaded successfully"))
  (error
   (message "✗ Multiplexer failed: %s" err)
   (message "Error details: %s" (error-message-string err))))
