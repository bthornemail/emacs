;;; simple-wave-example.el --- Simple wave function example

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: example, wave-function, demonstration
;; Package-Requires: ((emacs "27.1") (wave-function-core "1.0") (wave-function-engine "1.0") (wave-emacs-integration "1.0"))

;;; Commentary:
;; This example demonstrates basic wave function creation and manipulation:
;; - Create a simple wave function with Church encoding
;; - Display it in an Emacs buffer
;; - Show wave interference patterns
;; - Demonstrate basic geometric constraints

;;; Code:

(require 'wave-function-core)
(require 'wave-function-engine)
(require 'wave-emacs-integration)

;;; Simple Wave Function Example

(defun simple-wave-example ()
  "Demonstrate basic wave function creation and manipulation"
  (interactive)
  
  (message "Creating simple wave function example...")
  
  ;; Create a simple wave function using Church encoding
  (let* ((wave-id "simple-wave-001")
         (base-frequency 440.0)  ; A4 note
         (amplitude 0.8)
         (phase 0.0)
         (harmonics '(880.0 1320.0))  ; Octave and fifth
         (wave-function (create-wave-function-church wave-id base-frequency amplitude phase harmonics)))
    
    ;; Display the wave function in a buffer
    (wave-function-create-and-display-buffer wave-function 'core-wave-mode)
    
    ;; Show wave function details
    (message "Created wave function: %s" (identity-wave-function-id wave-function))
    (message "Base frequency: %f Hz" (identity-wave-function-base-frequency wave-function))
    (message "Amplitude: %f" (identity-wave-function-amplitude wave-function))
    (message "Phase: %f radians" (identity-wave-function-phase wave-function))
    (message "Harmonics: %S" (identity-wave-function-harmonics wave-function))
    
    ;; Demonstrate wave interference
    (simple-wave-interference-example wave-function)
    
    wave-function))

(defun simple-wave-interference-example (wave1)
  "Demonstrate wave interference with a second wave"
  (let* ((wave2-id "simple-wave-002")
         (wave2 (create-wave-function-church wave2-id 550.0 0.6 0.5 '(1100.0 1650.0))))
    
    (message "Creating second wave for interference demonstration...")
    (wave-function-create-and-display-buffer wave2 'core-wave-mode)
    
    ;; Calculate interference
    (let ((interference-result (calculate-wave-interference-church wave1 wave2)))
      (message "Wave Interference Result:")
      (message "  Resultant Frequency: %f Hz" (plist-get interference-result :resultant-frequency))
      (message "  Resultant Amplitude: %f" (plist-get interference-result :resultant-amplitude))
      (message "  Resultant Phase: %f radians" (plist-get interference-result :resultant-phase))
      
      ;; Create a buffer to display interference result
      (let ((buffer (get-buffer-create "*wave-interference-result*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "Wave Interference Analysis\n")
          (insert "========================\n\n")
          (insert (format "Wave 1: %s (%.1f Hz, %.2f amplitude)\n" 
                          (identity-wave-function-id wave1)
                          (identity-wave-function-base-frequency wave1)
                          (identity-wave-function-amplitude wave1)))
          (insert (format "Wave 2: %s (%.1f Hz, %.2f amplitude)\n\n" 
                          (identity-wave-function-id wave2)
                          (identity-wave-function-base-frequency wave2)
                          (identity-wave-function-amplitude wave2)))
          (insert "Interference Result:\n")
          (insert (format "  Resultant Frequency: %.1f Hz\n" (plist-get interference-result :resultant-frequency)))
          (insert (format "  Resultant Amplitude: %.3f\n" (plist-get interference-result :resultant-amplitude)))
          (insert (format "  Resultant Phase: %.3f radians\n" (plist-get interference-result :resultant-phase)))
          (insert "\nThis demonstrates how two wave functions combine\n")
          (insert "to create a new wave with different properties.\n"))
        (switch-to-buffer buffer)))
    
    wave2))

;;; Church Encoding Demonstration

(defun church-encoding-demo ()
  "Demonstrate Church encoding for numbers"
  (interactive)
  
  (message "Church Encoding Demonstration")
  
  ;; Create Church numerals
  (let ((church-0 #'church-zero)
        (church-1 #'church-one)
        (church-2 #'church-two)
        (church-3 #'church-three))
    
    ;; Convert to integers
    (message "Church Zero: %d" (church-to-int church-0))
    (message "Church One: %d" (church-to-int church-1))
    (message "Church Two: %d" (church-to-int church-2))
    (message "Church Three: %d" (church-to-int church-3))
    
    ;; Demonstrate Church arithmetic
    (let ((church-5 (church-add church-2 church-3))
          (church-6 (church-mult church-2 church-3)))
      (message "Church 2 + 3 = %d" (church-to-int church-5))
      (message "Church 2 * 3 = %d" (church-to-int church-6)))
    
    ;; Show how Church encoding is used in wave functions
    (let ((wave-freq-church (int-to-church 440))
          (wave-amp-church (int-to-church 80)))  ; 0.8 * 100
      (message "Wave frequency as Church numeral: %d" (church-to-int wave-freq-church))
      (message "Wave amplitude as Church numeral: %d" (church-to-int wave-amp-church)))))

;;; Geometric Constraints Example

(defun geometric-constraints-example ()
  "Demonstrate geometric constraints on wave functions"
  (interactive)
  
  (message "Geometric Constraints Example")
  
  ;; Create a wave function
  (let* ((wave (create-wave-function-church "geometric-wave" 100.0 0.5 0.0 nil))
         (tetrahedron-shape (make-geometric-shape :name "tetrahedron" :vertices 4 :edges 6 :faces 4)))
    
    (message "Original wave frequency: %f" (identity-wave-function-base-frequency wave))
    
    ;; Apply geometric constraints
    (let ((constrained-wave (apply-geometric-wave-constraints wave tetrahedron-shape)))
      (message "Constrained wave frequency: %f" (identity-wave-function-base-frequency constrained-wave))
      (message "Constrained wave harmonics: %S" (identity-wave-function-harmonics constrained-wave))
      
      ;; Display the constrained wave
      (wave-function-create-and-display-buffer constrained-wave 'core-wave-mode))))

;;; Interactive Demo Function

(defun run-simple-wave-demo ()
  "Run the complete simple wave function demonstration"
  (interactive)
  
  (message "=== Simple Wave Function Demo ===")
  
  ;; Step 1: Church encoding demo
  (message "\n1. Church Encoding Demonstration:")
  (church-encoding-demo)
  
  ;; Step 2: Simple wave creation
  (message "\n2. Simple Wave Function Creation:")
  (let ((wave (simple-wave-example)))
    
    ;; Step 3: Geometric constraints
    (message "\n3. Geometric Constraints:")
    (geometric-constraints-example)
    
    ;; Step 4: Show final result
    (message "\n=== Demo Complete ===")
    (message "Created wave function: %s" (identity-wave-function-id wave))
    (message "Check the wave function buffers for visualizations!")))

;;; Keybindings for the example

(defvar simple-wave-example-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'run-simple-wave-demo)
    (define-key map (kbd "C-c C-c") 'church-encoding-demo)
    (define-key map (kbd "C-c C-g") 'geometric-constraints-example)
    map)
  "Keymap for simple wave example mode")

(define-minor-mode simple-wave-example-mode
  "Minor mode for simple wave function examples"
  :lighter " SimpleWave"
  :keymap simple-wave-example-mode-map)

(provide 'simple-wave-example)

;;; simple-wave-example.el ends here
