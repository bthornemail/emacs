;;; test-startup.el --- Test script for wave function package startup

;; This script tests the wave function package components step by step

;; Load the minimal startup workflow
(load-file "minimal-startup.el")

;; Run the minimal startup
(wave-function-minimal-startup)

;; If successful, test basic functionality
(wave-function-test-basic-functionality)

;; Enter debug mode
(wave-function-debug-mode)
