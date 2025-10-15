;;; corrected-church-test.el --- Corrected Church Encoding Test

;; Test Church encoding with proper execution

(load-file "wave-function-core.el")
(load-file "wave-geometric-solids.el")
(load-file "wave-archimedean.el")
(load-file "wave-function-engine.el")

(message "=== CORRECTED CHURCH ENCODING TEST ===")

;; Test Church Zero
(message "Testing Church Zero...")
(let ((church-0 (church-zero))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((result (funcall (funcall church-0 test-fn) test-value)))
    (message "Church Zero result: %s (expected: 0)" result)
    (if (= result 0)
        (message "✓ Church Zero: PASSED")
      (message "✗ Church Zero: FAILED"))))

;; Test Church One - Corrected approach
(message "Testing Church One...")
(let ((church-1 (church-one))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((church-1-applied (funcall church-1 test-fn)))
    (let ((result (funcall church-1-applied test-value)))
      (message "Church One result: %s (expected: 1)" result)
      (if (= result 1)
          (message "✓ Church One: PASSED")
        (message "✗ Church One: FAILED")))))

;; Test Church Two - Corrected approach
(message "Testing Church Two...")
(let ((church-2 (church-two))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((church-2-applied (funcall church-2 test-fn)))
    (let ((result (funcall church-2-applied test-value)))
      (message "Church Two result: %s (expected: 2)" result)
      (if (= result 2)
          (message "✓ Church Two: PASSED")
        (message "✗ Church Two: FAILED")))))

;; Test Church Numerals - Corrected approach
(message "Testing Church Numerals...")
(let ((church-5 (church-n 5))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((church-5-applied (funcall church-5 test-fn)))
    (let ((result (funcall church-5-applied test-value)))
      (message "Church 5 result: %s (expected: 5)" result)
      (if (= result 5)
          (message "✓ Church 5: PASSED")
        (message "✗ Church 5: FAILED")))))

;; Test Golden Ratio
(message "Testing Golden Ratio...")
(let ((phi (/ (+ 1 (sqrt 5)) 2))
      (phi-squared (* phi phi))
      (phi-plus-one (+ phi 1))
      (tolerance 1e-10))
  (let ((diff (abs (- phi-squared phi-plus-one))))
    (message "φ = %s" phi)
    (message "φ² = %s" phi-squared)
    (message "φ + 1 = %s" phi-plus-one)
    (message "Difference: %s" diff)
    (if (< diff tolerance)
        (message "✓ Golden Ratio φ² = φ + 1: PASSED")
      (message "✗ Golden Ratio φ² = φ + 1: FAILED"))))

;; Test Trigonometric Identity
(message "Testing Trigonometric Identity...")
(let ((theta (/ pi 6))
      (sin-val (sin theta))
      (cos-val (cos theta))
      (sum-squares (+ (* sin-val sin-val) (* cos-val cos-val)))
      (expected 1.0)
      (tolerance 1e-10))
  (let ((diff (abs (- sum-squares expected))))
    (message "θ = π/6")
    (message "sin(θ) = %s" sin-val)
    (message "cos(θ) = %s" cos-val)
    (message "sin²(θ) + cos²(θ) = %s" sum-squares)
    (message "Difference: %s" diff)
    (if (< diff tolerance)
        (message "✓ Trigonometric Identity sin² + cos² = 1: PASSED")
      (message "✗ Trigonometric Identity sin² + cos² = 1: FAILED"))))

;; Test Wave Function Creation
(message "Testing Wave Function Creation...")
(let ((wave (create-wave-function-church "test-wave" 440.0 1.0 0.0)))
  (if wave
      (progn
        (message "Wave ID: %s" (identity-wave-function-id wave))
        (message "Wave Frequency: %s Hz" (identity-wave-function-base-frequency wave))
        (message "Wave Amplitude: %s" (identity-wave-function-amplitude wave))
        (message "Wave Phase: %s" (identity-wave-function-phase wave))
        (message "✓ Wave Function Creation: PASSED"))
    (message "✗ Wave Function Creation: FAILED")))

;; Test 5-Cell Creation
(message "Testing 5-Cell Creation...")
(let ((five-cell (wave-function-create-5-cell)))
  (if five-cell
      (progn
        (message "5-Cell Name: %s" (geometric-shape-name five-cell))
        (message "5-Cell Vertices: %d" (length (geometric-shape-vertices five-cell)))
        (message "5-Cell Edges: %d" (length (geometric-shape-edges five-cell)))
        (message "5-Cell Faces: %d" (length (geometric-shape-faces five-cell)))
        (message "5-Cell Use Case: %s" (geometric-shape-use-case five-cell))
        (message "5-Cell Consciousness Level: %s" (geometric-shape-consciousness-level five-cell))
        (message "✓ 5-Cell Creation: PASSED"))
    (message "✗ 5-Cell Creation: FAILED")))

(message "=== CORRECTED CHURCH ENCODING TEST COMPLETE ===")
