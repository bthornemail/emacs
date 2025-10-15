;;; interactive-test.el --- Interactive testing session for wave function package

;; This script provides an interactive testing environment

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load core components
(message "=== Loading Core Components ===")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-archimedean.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")

;; Load server and client
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-server.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-client.el")

(message "✓ All components loaded successfully")

;; Start server and connect client
(wave-server-start)
(wave-client-connect)

;; Interactive testing functions
(defun test-geometric-shapes-interactive ()
  "Interactive test of geometric shapes"
  (interactive)
  (message "=== Interactive Geometric Shapes Test ===")
  (condition-case err
      (progn
        (let ((tetrahedron (wave-function-create-platonic-solid 'tetrahedron)))
          (message "✓ Tetrahedron created successfully!")
          (message "  Name: %s" (geometric-shape-name tetrahedron))
          (message "  Vertices: %d" (geometric-shape-vertices tetrahedron))
          (message "  Edges: %d" (geometric-shape-edges tetrahedron))
          (message "  Faces: %d" (geometric-shape-faces tetrahedron))
          (message "  Face-vertex ratio: %s" (geometric-shape-face-vertex-ratio tetrahedron))
          (message "  Use case: %s" (geometric-shape-use-case tetrahedron))))
    (error
     (message "✗ Error creating tetrahedron: %s" err))))

(defun test-archimedean-solids-interactive ()
  "Interactive test of Archimedean solids"
  (interactive)
  (message "=== Interactive Archimedean Solids Test ===")
  (condition-case err
      (progn
        (let ((cuboctahedron (wave-function-create-archimedean-solid 'cuboctahedron)))
          (message "✓ Cuboctahedron created successfully!")
          (message "  Name: %s" (geometric-shape-name cuboctahedron))
          (message "  Vertices: %d" (geometric-shape-vertices cuboctahedron))
          (message "  Edges: %d" (geometric-shape-edges cuboctahedron))
          (message "  Faces: %d" (geometric-shape-faces cuboctahedron))
          (message "  Consciousness level: %s" (geometric-shape-consciousness-level cuboctahedron))))
    (error
     (message "✗ Error creating cuboctahedron: %s" err))))

(defun test-5-cell-interactive ()
  "Interactive test of 5-cell"
  (interactive)
  (message "=== Interactive 5-Cell Test ===")
  (condition-case err
      (progn
        (let ((five-cell (wave-function-create-5-cell)))
          (message "✓ 5-cell created successfully!")
          (message "  Name: %s" (geometric-shape-name five-cell))
          (message "  Vertices: %d" (length (geometric-shape-vertices five-cell)))
          (message "  Edges: %d" (length (geometric-shape-edges five-cell)))
          (message "  Faces: %d" (length (geometric-shape-faces five-cell)))
          (message "  Face-vertex ratio: %s" (geometric-shape-face-vertex-ratio five-cell))))
    (error
     (message "✗ Error creating 5-cell: %s" err))))

(defun test-incidence-matrices-interactive ()
  "Interactive test of incidence matrices"
  (interactive)
  (message "=== Interactive Incidence Matrices Test ===")
  (condition-case err
      (progn
        (let ((tetrahedron-matrix (wave-function-create-incidence-matrix 'tetrahedron)))
          (message "✓ Tetrahedron incidence matrix created!")
          (message "  Matrix size: %dx%d" (length tetrahedron-matrix) (length (aref tetrahedron-matrix 0))))
        (let ((five-cell-matrix (wave-function-create-5-cell-incidence-matrix)))
          (message "✓ 5-cell incidence matrix created!")
          (message "  Matrix size: %dx%d" (length five-cell-matrix) (length (aref five-cell-matrix 0)))))
    (error
     (message "✗ Error creating incidence matrices: %s" err))))

(defun test-betti-numbers-interactive ()
  "Interactive test of Betti numbers"
  (interactive)
  (message "=== Interactive Betti Numbers Test ===")
  (condition-case err
      (progn
        (let ((tetrahedron-betti (wave-function-calculate-betti-numbers 'tetrahedron)))
          (message "✓ Tetrahedron Betti numbers: %s" tetrahedron-betti))
        (let ((cube-betti (wave-function-calculate-betti-numbers 'cube)))
          (message "✓ Cube Betti numbers: %s" cube-betti))
        (let ((icosahedron-betti (wave-function-calculate-betti-numbers 'icosahedron)))
          (message "✓ Icosahedron Betti numbers: %s" icosahedron-betti)))
    (error
     (message "✗ Error calculating Betti numbers: %s" err))))

(defun test-server-client-interactive ()
  "Interactive test of server-client communication"
  (interactive)
  (message "=== Interactive Server-Client Test ===")
  (wave-client-ping-server)
  (wave-client-get-server-status)
  (wave-client-test-component "wave-geometric-solids"))

(defun run-all-interactive-tests ()
  "Run all interactive tests"
  (interactive)
  (message "=== Running All Interactive Tests ===")
  (test-geometric-shapes-interactive)
  (message "")
  (test-archimedean-solids-interactive)
  (message "")
  (test-5-cell-interactive)
  (message "")
  (test-incidence-matrices-interactive)
  (message "")
  (test-betti-numbers-interactive)
  (message "")
  (test-server-client-interactive)
  (message "")
  (message "=== All Interactive Tests Complete ==="))

;; Show available commands
(message "=== Interactive Testing Environment Ready ===")
(message "Available interactive commands:")
(message "  M-x test-geometric-shapes-interactive")
(message "  M-x test-archimedean-solids-interactive")
(message "  M-x test-5-cell-interactive")
(message "  M-x test-incidence-matrices-interactive")
(message "  M-x test-betti-numbers-interactive")
(message "  M-x test-server-client-interactive")
(message "  M-x run-all-interactive-tests")
(message "")
(message "Server status: %s" (if wave-server-running "Running" "Stopped"))
(message "Client status: %s" (if wave-client-connected "Connected" "Disconnected"))
(message "")
(message "Ready for interactive testing!")

(provide 'interactive-test)
