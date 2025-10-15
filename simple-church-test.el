;;; simple-church-test.el --- Simple Church Encoding Test

;; Test Church encoding with proper execution

(load-file "wave-function-core.el")
(load-file "wave-geometric-solids.el")
(load-file "wave-archimedean.el")
(load-file "wave-function-engine.el")

(message "=== SIMPLE CHURCH ENCODING TEST ===")

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

;; Test Church One
(message "Testing Church One...")
(let ((church-1 (church-one))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((result (funcall (funcall church-1 test-fn) test-value)))
    (message "Church One result: %s (expected: 1)" result)
    (if (= result 1)
        (message "✓ Church One: PASSED")
      (message "✗ Church One: FAILED"))))

;; Test Church Two
(message "Testing Church Two...")
(let ((church-2 (church-two))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((result (funcall (funcall church-2 test-fn) test-value)))
    (message "Church Two result: %s (expected: 2)" result)
    (if (= result 2)
        (message "✓ Church Two: PASSED")
      (message "✗ Church Two: FAILED"))))

;; Test Church Numerals
(message "Testing Church Numerals...")
(let ((church-5 (church-n 5))
      (test-fn (lambda (x) (+ x 1)))
      (test-value 0))
  (let ((result (funcall (funcall church-5 test-fn) test-value)))
    (message "Church 5 result: %s (expected: 5)" result)
    (if (= result 5)
        (message "✓ Church 5: PASSED")
      (message "✗ Church 5: FAILED"))))

(message "=== CHURCH ENCODING TEST COMPLETE ===")
