;;; minimal-init.el --- Minimal Emacs initialization for wave function testing

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load the minimal startup workflow
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/minimal-startup.el")

;; Run the minimal startup test
(wave-function-minimal-startup)

;; If we get here, the startup was successful
(message "Wave function package startup completed successfully!")
