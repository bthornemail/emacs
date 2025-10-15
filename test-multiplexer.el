;;; test-multiplexer.el --- Test multiplexer file syntax

;; Test the multiplexer file syntax
(condition-case err
    (progn
      (load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-multiplexer.el")
      (message "✓ Multiplexer loaded successfully"))
  (error
   (message "✗ Multiplexer failed: %s" err)
   (message "Error details: %s" (error-message-string err))))
