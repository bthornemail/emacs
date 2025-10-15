;;; test-client-only.el --- Client-only testing without server

;; This script tests the wave function package components directly
;; without requiring a server connection

;; Add the wave function package directory to load path
(add-to-list 'load-path "/home/main/dev/Axiomatic/demos/emacs-demo")

;; Load core components
(message "=== Loading Core Components ===")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-core.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-geometric-solids.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-archimedean.el")
(load-file "/home/main/dev/Axiomatic/demos/emacs-demo/wave-function-engine.el")

(message "✓ Core components loaded successfully")

;; Direct testing functions
(defun test-geometric-shapes-direct ()
  "Direct test of geometric shapes"
  (message "=== Direct Geometric Shapes Test ===")
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

(defun test-archimedean-solids-direct ()
  "Direct test of Archimedean solids"
  (message "=== Direct Archimedean Solids Test ===")
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

(defun test-5-cell-direct ()
  "Direct test of 5-cell"
  (message "=== Direct 5-Cell Test ===")
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

(defun test-incidence-matrices-direct ()
  "Direct test of incidence matrices"
  (message "=== Direct Incidence Matrices Test ===")
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

(defun test-betti-numbers-direct ()
  "Direct test of Betti numbers"
  (message "=== Direct Betti Numbers Test ===")
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

(defun test-edge-generation-direct ()
  "Direct test of edge generation"
  (message "=== Direct Edge Generation Test ===")
  (condition-case err
      (progn
        (let ((tetrahedron-edges (wave-function-generate-edges 'tetrahedron)))
          (message "✓ Tetrahedron edges generated: %d edges" (length tetrahedron-edges))
          (message "  First few edges: %s" (butlast tetrahedron-edges 3)))
        (let ((cube-edges (wave-function-generate-edges 'cube)))
          (message "✓ Cube edges generated: %d edges" (length cube-edges))
          (message "  First few edges: %s" (butlast cube-edges 9))))
    (error
     (message "✗ Error generating edges: %s" err))))

(defun test-face-generation-direct ()
  "Direct test of face generation"
  (message "=== Direct Face Generation Test ===")
  (condition-case err
      (progn
        (let ((tetrahedron-faces (wave-function-generate-faces 'tetrahedron)))
          (message "✓ Tetrahedron faces generated: %d faces" (length tetrahedron-faces))
          (message "  Faces: %s" tetrahedron-faces))
        (let ((cube-faces (wave-function-generate-faces 'cube)))
          (message "✓ Cube faces generated: %d faces" (length cube-faces))
          (message "  First few faces: %s" (butlast cube-faces 3))))
    (error
     (message "✗ Error generating faces: %s" err))))

(defun run-all-direct-tests ()
  "Run all direct tests"
  (message "=== Running All Direct Tests ===")
  (test-geometric-shapes-direct)
  (message "")
  (test-archimedean-solids-direct)
  (message "")
  (test-5-cell-direct)
  (message "")
  (test-incidence-matrices-direct)
  (message "")
  (test-betti-numbers-direct)
  (message "")
  (test-edge-generation-direct)
  (message "")
  (test-face-generation-direct)
  (message "")
  (message "=== All Direct Tests Complete ==="))

;; Run all tests
(run-all-direct-tests)

(message "=== Client-Only Test Complete ===")
