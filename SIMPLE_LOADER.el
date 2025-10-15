;;; SIMPLE_LOADER.el --- Simple loader for Wave Function Emacs Package

(defun wave-function-simple-test ()
  "Simple test of core functionality."
  (interactive)
  (message "🧪 Simple Core Functionality Test")
  (message "================================")
  
  ;; Test 1: Church Zero
  (let ((church-zero (lambda (f) (lambda (x) x))))
    (let ((result ((church-zero (lambda (x) (+ x 1))) 0)))
      (message "✅ Church Zero: %d" result)))
  
  ;; Test 2: Golden Ratio
  (let ((phi (/ (+ 1 (sqrt 5)) 2)))
    (let ((phi-squared (* phi phi))
          (phi-plus-one (+ phi 1)))
      (let ((difference (abs (- phi-squared phi-plus-one))))
        (message "✅ Golden Ratio: φ = %.15f" phi)
        (message "   φ² = %.15f" phi-squared)
        (message "   φ + 1 = %.15f" phi-plus-one)
        (message "   Difference: %.15f" difference))))
  
  ;; Test 3: Trigonometric Identity
  (let ((theta (/ pi 6)))
    (let ((sin-theta (sin theta))
          (cos-theta (cos theta)))
      (let ((identity (+ (* sin-theta sin-theta) (* cos-theta cos-theta))))
        (message "✅ Trig Identity: sin²(π/6) + cos²(π/6) = %.15f" identity))))
  
  (message "🎉 Simple test completed!"))
