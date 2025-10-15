;;; wave-incidence-workflow.el --- Incidence-based workflow definitions

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: incidence, workflow, graph, vertices, edges, geometric, wave-function
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-y-combinator "1.0") (wave-async-framework "1.0"))

;;; Commentary:
;; Implements incidence-based workflow definitions using graph theory:
;; - Vertices: Operations/states in the workflow
;; - Edges: Transitions/flows between operations
;; - Incidence Matrix: V x E matrix defining vertex-edge relationships
;; - Geometric Constraints: Fano plane, 5-cell, golden ratio validation
;; - SPO-Modality: Subject-Predicate-Object-Modality framework
;; - YAML Configuration: Load workflows from YAML with incidence relations

;;; Code:

(require 'cl-lib)
(require 'wave-y-combinator)
(require 'wave-async-framework)

;;; Incidence Workflow Data Structures

(cl-defstruct wave-incidence-workflow
  "Incidence-based workflow definition using graph theory"
  (workflow-id "" :type string)
  (vertices nil :type list)  ; List of vertex structures
  (edges nil :type list)     ; List of edge structures
  (incidence-matrix nil)     ; V x E matrix
  (geometric-constraints nil :type list)
  (spo-modality nil :type (or plist null))
  (y-combinator-level 'shared :type symbol)
  (execution-strategy 'depth-first :type symbol)  ; depth-first, breadth-first, topological
  (context nil :type (or plist null)))

(cl-defstruct wave-incidence-vertex
  "Vertex in incidence workflow graph"
  (vertex-id "" :type string)
  (operation nil :type (or function string))
  (operation-type 'input :type symbol)  ; input, eval, output, control, async
  (async-enabled nil :type boolean)
  (y-combinator-level 'private :type symbol)
  (geometric-position nil :type (or list null))
  (incidence-relations nil :type list)
  (state 'pending :type symbol)
  (result nil)
  (error nil :type (or string null))
  (execution-time 0.0 :type float))

(cl-defstruct wave-incidence-edge
  "Edge in incidence workflow graph"
  (edge-id "" :type string)
  (from-vertex "" :type string)
  (to-vertex "" :type string)
  (condition nil :type (or function string))
  (condition-met nil :type boolean)
  (async-enabled nil :type boolean)
  (y-combinator-level 'private :type symbol)
  (geometric-constraint nil :type (or list null))
  (transition-time 0.0 :type float))

(cl-defstruct wave-incidence-execution-context
  "Context for incidence workflow execution"
  (workflow-id "" :type string)
  (execution-id "" :type string)
  (current-vertex nil :type (or string null))
  (visited-vertices nil :type list)
  (execution-path nil :type list)
  (vertex-results (make-hash-table :test 'equal) :type hash-table)
  (edge-conditions (make-hash-table :test 'equal) :type hash-table)
  (state 'initialized :type symbol)
  (start-time nil :type (or float null))
  (geometric-state nil :type (or plist null))
  (spo-context nil :type (or plist null)))

;;; YAML Parsing for Incidence Workflows

(defun wave-incidence-workflow-from-yaml (yaml-file)
  "Load incidence-based workflow from YAML file"
  (when (file-exists-p yaml-file)
    (let ((yaml-content (with-temp-buffer
                          (insert-file-contents yaml-file)
                          (buffer-string))))
      (wave-incidence-workflow-parse-yaml yaml-content))))

(defun wave-incidence-workflow-parse-yaml (yaml-content)
  "Parse YAML content into incidence workflow structure"
  (let ((workflow (make-wave-incidence-workflow))
        (lines (split-string yaml-content "\n" t)))
    
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "^incidence_workflow:" trimmed)
          (setq in-workflow-section t))
         ((string-match "^id:" trimmed)
          (setf (wave-incidence-workflow-workflow-id workflow)
                (string-trim (substring trimmed 3))))
         ((string-match "^vertices:" trimmed)
          (wave-incidence-parse-vertices workflow lines))
         ((string-match "^edges:" trimmed)
          (wave-incidence-parse-edges workflow lines))
         ((string-match "^incidence_matrix:" trimmed)
          (wave-incidence-parse-incidence-matrix workflow lines))
         ((string-match "^spo_modality:" trimmed)
          (wave-incidence-parse-spo-modality workflow lines)))))
    
    workflow))

(defun wave-incidence-parse-vertices (workflow lines)
  "Parse vertices section from YAML"
  (let ((in-vertices-section t)
        (current-vertex nil)
        (vertices nil))
    
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "^vertices:" trimmed)
          (setq in-vertices-section t))
         ((and in-vertices-section (string-match "^- id:" trimmed))
          (when current-vertex
            (push current-vertex vertices))
          (setq current-vertex (make-wave-incidence-vertex
                                :vertex-id (string-trim (substring trimmed 5)))))
         ((and in-vertices-section current-vertex (string-match "operation:" trimmed))
          (setf (wave-incidence-vertex-operation current-vertex)
                (string-trim (substring trimmed 10))))
         ((and in-vertices-section current-vertex (string-match "type:" trimmed))
          (setf (wave-incidence-vertex-operation-type current-vertex)
                (intern (string-trim (substring trimmed 5)))))
         ((and in-vertices-section current-vertex (string-match "async:" trimmed))
          (setf (wave-incidence-vertex-async-enabled current-vertex)
                (string= (string-trim (substring trimmed 6)) "true")))
         ((and in-vertices-section current-vertex (string-match "y_combinator:" trimmed))
          (setf (wave-incidence-vertex-y-combinator-level current-vertex)
                (intern (string-trim (substring trimmed 12)))))
         ((and in-vertices-section (string-match "^[a-zA-Z]" trimmed))
          (setq in-vertices-section nil)))))
    
    (when current-vertex
      (push current-vertex vertices))
    
    (setf (wave-incidence-workflow-vertices workflow) (nreverse vertices))))

(defun wave-incidence-parse-edges (workflow lines)
  "Parse edges section from YAML"
  (let ((in-edges-section t)
        (current-edge nil)
        (edges nil))
    
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "^edges:" trimmed)
          (setq in-edges-section t))
         ((and in-edges-section (string-match "^- id:" trimmed))
          (when current-edge
            (push current-edge edges))
          (setq current-edge (make-wave-incidence-edge
                              :edge-id (string-trim (substring trimmed 5)))))
         ((and in-edges-section current-edge (string-match "from:" trimmed))
          (setf (wave-incidence-edge-from-vertex current-edge)
                (string-trim (substring trimmed 5))))
         ((and in-edges-section current-edge (string-match "to:" trimmed))
          (setf (wave-incidence-edge-to-vertex current-edge)
                (string-trim (substring trimmed 3))))
         ((and in-edges-section current-edge (string-match "condition:" trimmed))
          (setf (wave-incidence-edge-condition current-edge)
                (string-trim (substring trimmed 10))))
         ((and in-edges-section current-edge (string-match "async:" trimmed))
          (setf (wave-incidence-edge-async-enabled current-edge)
                (string= (string-trim (substring trimmed 6)) "true")))
         ((and in-edges-section current-edge (string-match "y_combinator:" trimmed))
          (setf (wave-incidence-edge-y-combinator-level current-edge)
                (intern (string-trim (substring trimmed 12)))))
         ((and in-edges-section (string-match "^[a-zA-Z]" trimmed))
          (setq in-edges-section nil)))))
    
    (when current-edge
      (push current-edge edges))
    
    (setf (wave-incidence-workflow-edges workflow) (nreverse edges))))

(defun wave-incidence-parse-incidence-matrix (workflow lines)
  "Parse incidence matrix from YAML"
  (let ((in-matrix-section t)
        (rows nil)
        (cols nil)
        (data nil))
    
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "^incidence_matrix:" trimmed)
          (setq in-matrix-section t))
         ((and in-matrix-section (string-match "rows:" trimmed))
          (setq rows (wave-incidence-parse-list (string-trim (substring trimmed 5)))))
         ((and in-matrix-section (string-match "cols:" trimmed))
          (setq cols (wave-incidence-parse-list (string-trim (substring trimmed 5)))))
         ((and in-matrix-section (string-match "data:" trimmed))
          (setq data (wave-incidence-parse-matrix-data lines)))
         ((and in-matrix-section (string-match "^[a-zA-Z]" trimmed))
          (setq in-matrix-section nil)))))
    
    (setf (wave-incidence-workflow-incidence-matrix workflow)
          (list :rows rows :cols cols :data data))))

(defun wave-incidence-parse-spo-modality (workflow lines)
  "Parse SPO modality from YAML"
  (let ((in-spo-section t)
        (spo-plist nil))
    
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "^spo_modality:" trimmed)
          (setq in-spo-section t))
         ((and in-spo-section (string-match "subject:" trimmed))
          (setq spo-plist (plist-put spo-plist :subject (string-trim (substring trimmed 8)))))
         ((and in-spo-section (string-match "predicate:" trimmed))
          (setq spo-plist (plist-put spo-plist :predicate (string-trim (substring trimmed 10)))))
         ((and in-spo-section (string-match "object:" trimmed))
          (setq spo-plist (plist-put spo-plist :object (string-trim (substring trimmed 7)))))
         ((and in-spo-section (string-match "modality:" trimmed))
          (setq spo-plist (plist-put spo-plist :modality (intern (string-trim (substring trimmed 9))))))
         ((and in-spo-section (string-match "^[a-zA-Z]" trimmed))
          (setq in-spo-section nil)))))
    
    (setf (wave-incidence-workflow-spo-modality workflow) spo-plist)))

(defun wave-incidence-parse-list (list-str)
  "Parse list from string like [\"v0\", \"v1\", \"v2\"]"
  (let ((cleaned (string-trim list-str "[]")))
    (mapcar (lambda (item) (string-trim item "\""))
            (split-string cleaned "," t))))

(defun wave-incidence-parse-matrix-data (lines)
  "Parse matrix data from YAML lines"
  (let ((data nil)
        (in-data-section nil))
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-match "data:" trimmed)
          (setq in-data-section t))
         ((and in-data-section (string-match "^- \\[" trimmed))
          (let ((row-data (wave-incidence-parse-list (string-trim (substring trimmed 2)))))
            (push (mapcar 'string-to-number row-data) data)))
         ((and in-data-section (string-match "^[a-zA-Z]" trimmed))
          (setq in-data-section nil)))))
    (nreverse data)))

;;; Incidence Matrix Operations

(defun wave-incidence-matrix-get (workflow vertex-id edge-id)
  "Get incidence matrix value for vertex-edge pair"
  (let ((matrix (wave-incidence-workflow-incidence-matrix workflow)))
    (when matrix
      (let ((rows (plist-get matrix :rows))
            (cols (plist-get matrix :cols))
            (data (plist-get matrix :data)))
        (when (and rows cols data)
          (let ((row-index (position vertex-id rows :test 'string=))
                (col-index (position edge-id cols :test 'string=)))
            (when (and row-index col-index
                       (< row-index (length data))
                       (< col-index (length (nth row-index data))))
              (nth col-index (nth row-index data)))))))))

(defun wave-incidence-matrix-validate (workflow)
  "Validate incidence matrix against vertices and edges"
  (let ((vertices (wave-incidence-workflow-vertices workflow))
        (edges (wave-incidence-workflow-edges workflow))
        (matrix (wave-incidence-workflow-incidence-matrix workflow))
        (valid t))
    
    (when matrix
      (let ((rows (plist-get matrix :rows))
            (cols (plist-get matrix :cols))
            (data (plist-get matrix :data)))
        
        ;; Check that all vertices are in rows
        (dolist (vertex vertices)
          (unless (member (wave-incidence-vertex-vertex-id vertex) rows)
            (setq valid nil)
            (message "Vertex %s not found in incidence matrix rows" 
                     (wave-incidence-vertex-vertex-id vertex))))
        
        ;; Check that all edges are in cols
        (dolist (edge edges)
          (unless (member (wave-incidence-edge-edge-id edge) cols)
            (setq valid nil)
            (message "Edge %s not found in incidence matrix cols" 
                     (wave-incidence-edge-edge-id edge))))
        
        ;; Check matrix dimensions
        (when (and data rows cols)
          (unless (= (length data) (length rows))
            (setq valid nil)
            (message "Matrix data rows (%d) != vertex count (%d)" 
                     (length data) (length rows)))
          
          (dolist (row data)
            (unless (= (length row) (length cols))
              (setq valid nil)
              (message "Matrix row length (%d) != edge count (%d)" 
                       (length row) (length cols)))))))
    
    valid))

;;; Workflow Execution

(defun wave-incidence-workflow-execute (workflow &optional context)
  "Execute incidence workflow following graph traversal"
  (let* ((execution-context (or context (wave-incidence-execution-context-create workflow)))
         (strategy (wave-incidence-workflow-execution-strategy workflow)))
    
    (setf (wave-incidence-execution-context-state execution-context) 'running)
    (setf (wave-incidence-execution-context-start-time execution-context) (float-time))
    
    (pcase strategy
      ('depth-first (wave-incidence-execute-depth-first workflow execution-context))
      ('breadth-first (wave-incidence-execute-breadth-first workflow execution-context))
      ('topological (wave-incidence-execute-topological workflow execution-context))
      (_ (error "Unknown execution strategy: %s" strategy)))))

(defun wave-incidence-execute-depth-first (workflow context)
  "Execute workflow using depth-first traversal"
  (let ((start-vertex (wave-incidence-find-start-vertex workflow)))
    (when start-vertex
      (wave-incidence-execute-vertex workflow start-vertex context)
      (wave-incidence-traverse-depth-first workflow start-vertex context))))

(defun wave-incidence-execute-breadth-first (workflow context)
  "Execute workflow using breadth-first traversal"
  (let ((start-vertex (wave-incidence-find-start-vertex workflow)))
    (when start-vertex
      (wave-incidence-traverse-breadth-first workflow start-vertex context))))

(defun wave-incidence-execute-topological (workflow context)
  "Execute workflow using topological ordering"
  (let ((topological-order (wave-incidence-topological-sort workflow)))
    (dolist (vertex-id topological-order)
      (let ((vertex (wave-incidence-find-vertex workflow vertex-id)))
        (when vertex
          (wave-incidence-execute-vertex workflow vertex context))))))

(defun wave-incidence-execute-vertex (workflow vertex context)
  "Execute a single vertex in the workflow"
  (let ((start-time (float-time)))
    (setf (wave-incidence-vertex-state vertex) 'running)
    (setf (wave-incidence-execution-context-current-vertex context) 
          (wave-incidence-vertex-vertex-id vertex))
    
    (condition-case err
        (let ((result (wave-incidence-execute-vertex-operation workflow vertex context)))
          (setf (wave-incidence-vertex-result vertex) result)
          (setf (wave-incidence-vertex-state vertex) 'completed)
          (puthash (wave-incidence-vertex-vertex-id vertex) result
                   (wave-incidence-execution-context-vertex-results context)))
      
      (error
       (setf (wave-incidence-vertex-error vertex) (error-message-string err))
       (setf (wave-incidence-vertex-state vertex) 'failed)
       (message "Vertex %s failed: %s" 
                (wave-incidence-vertex-vertex-id vertex) 
                (error-message-string err))))
    
    (setf (wave-incidence-vertex-execution-time vertex)
          (- (float-time) start-time))
    
    (push (wave-incidence-vertex-vertex-id vertex)
          (wave-incidence-execution-context-visited-vertices context))))

(defun wave-incidence-execute-vertex-operation (workflow vertex context)
  "Execute the operation associated with a vertex"
  (let ((operation (wave-incidence-vertex-operation vertex)))
    (cond
     ((functionp operation)
      (funcall operation context))
     ((stringp operation)
      (wave-incidence-execute-operation-string operation context))
     (t
      (error "Unknown operation type: %s" operation)))))

(defun wave-incidence-execute-operation-string (operation-str context)
  "Execute operation from string"
  (condition-case err
      (eval (read operation-str))
    (error
     (format "Operation execution error: %s" (error-message-string err)))))

;;; Graph Traversal Algorithms

(defun wave-incidence-traverse-depth-first (workflow current-vertex context)
  "Traverse workflow graph using depth-first search"
  (let ((outgoing-edges (wave-incidence-find-outgoing-edges workflow current-vertex)))
    (dolist (edge outgoing-edges)
      (when (wave-incidence-evaluate-edge-condition edge context)
        (let ((next-vertex (wave-incidence-find-vertex workflow 
                                                       (wave-incidence-edge-to-vertex edge))))
          (when (and next-vertex
                     (not (member (wave-incidence-vertex-vertex-id next-vertex)
                                  (wave-incidence-execution-context-visited-vertices context))))
            (wave-incidence-execute-vertex workflow next-vertex context)
            (wave-incidence-traverse-depth-first workflow next-vertex context)))))))

(defun wave-incidence-traverse-breadth-first (workflow start-vertex context)
  "Traverse workflow graph using breadth-first search"
  (let ((queue (list start-vertex))
        (visited nil))
    
    (while queue
      (let ((current-vertex (pop queue)))
        (unless (member (wave-incidence-vertex-vertex-id current-vertex) visited)
          (push (wave-incidence-vertex-vertex-id current-vertex) visited)
          (wave-incidence-execute-vertex workflow current-vertex context)
          
          (let ((outgoing-edges (wave-incidence-find-outgoing-edges workflow current-vertex)))
            (dolist (edge outgoing-edges)
              (when (wave-incidence-evaluate-edge-condition edge context)
                (let ((next-vertex (wave-incidence-find-vertex workflow 
                                                               (wave-incidence-edge-to-vertex edge))))
                  (when next-vertex
                    (push next-vertex queue)))))))))))

(defun wave-incidence-topological-sort (workflow)
  "Perform topological sort of workflow vertices"
  (let ((vertices (wave-incidence-workflow-vertices workflow))
        (edges (wave-incidence-workflow-edges workflow))
        (in-degree (make-hash-table :test 'equal))
        (result nil)
        (queue nil))
    
    ;; Calculate in-degrees
    (dolist (vertex vertices)
      (puthash (wave-incidence-vertex-vertex-id vertex) 0 in-degree))
    
    (dolist (edge edges)
      (let ((to-vertex (wave-incidence-edge-to-vertex edge)))
        (puthash to-vertex (1+ (gethash to-vertex in-degree 0)) in-degree)))
    
    ;; Find vertices with in-degree 0
    (dolist (vertex vertices)
      (when (= (gethash (wave-incidence-vertex-vertex-id vertex) in-degree) 0)
        (push (wave-incidence-vertex-vertex-id vertex) queue)))
    
    ;; Process queue
    (while queue
      (let ((current-vertex-id (pop queue)))
        (push current-vertex-id result)
        
        (dolist (edge edges)
          (when (string= (wave-incidence-edge-from-vertex edge) current-vertex-id)
            (let ((to-vertex (wave-incidence-edge-to-vertex edge)))
              (puthash to-vertex (1- (gethash to-vertex in-degree)) in-degree)
              (when (= (gethash to-vertex in-degree) 0)
                (push to-vertex queue)))))))
    
    (nreverse result)))

;;; Helper Functions

(defun wave-incidence-find-start-vertex (workflow)
  "Find the starting vertex (input type) in workflow"
  (cl-find-if (lambda (vertex)
                (eq (wave-incidence-vertex-operation-type vertex) 'input))
              (wave-incidence-workflow-vertices workflow)))

(defun wave-incidence-find-vertex (workflow vertex-id)
  "Find vertex by ID in workflow"
  (cl-find-if (lambda (vertex)
                (string= (wave-incidence-vertex-vertex-id vertex) vertex-id))
              (wave-incidence-workflow-vertices workflow)))

(defun wave-incidence-find-outgoing-edges (workflow vertex)
  "Find all outgoing edges from a vertex"
  (cl-remove-if-not (lambda (edge)
                      (string= (wave-incidence-edge-from-vertex edge)
                               (wave-incidence-vertex-vertex-id vertex)))
                    (wave-incidence-workflow-edges workflow)))

(defun wave-incidence-evaluate-edge-condition (edge context)
  "Evaluate edge condition to determine if transition should occur"
  (let ((condition (wave-incidence-edge-condition edge)))
    (cond
     ((functionp condition)
      (funcall condition context))
     ((stringp condition)
      (wave-incidence-evaluate-condition-string condition context))
     (t t))))

(defun wave-incidence-evaluate-condition-string (condition-str context)
  "Evaluate condition from string"
  (condition-case err
      (funcall (read condition-str) context)
    (error
     (message "Condition evaluation error: %s" (error-message-string err))
     nil)))

;;; Context Management

(defun wave-incidence-execution-context-create (workflow)
  "Create execution context for incidence workflow"
  (make-wave-incidence-execution-context
   :workflow-id (wave-incidence-workflow-workflow-id workflow)
   :execution-id (format "incidence-exec-%d" (random 10000))
   :state 'initialized
   :start-time (float-time)
   :geometric-state (wave-incidence-workflow-geometric-constraints workflow)
   :spo-context (wave-incidence-workflow-spo-modality workflow)))

;;; Registry and Management

(defvar wave-incidence-workflow-registry (make-hash-table :test 'equal)
  "Registry of incidence workflows by ID")

(defun wave-incidence-workflow-register (workflow)
  "Register incidence workflow in global registry"
  (puthash (wave-incidence-workflow-workflow-id workflow) workflow
           wave-incidence-workflow-registry)
  workflow)

(defun wave-incidence-workflow-get (workflow-id)
  "Get incidence workflow by ID from registry"
  (gethash workflow-id wave-incidence-workflow-registry))

;;; Inspection and Debug

(defun wave-incidence-workflow-inspect (workflow)
  "Inspect incidence workflow"
  (when (wave-incidence-workflow-p workflow)
    (message "Incidence Workflow: %s
  Vertices: %d
  Edges: %d
  Y-Combinator Level: %s
  Execution Strategy: %s
  SPO Modality: %S"
             (wave-incidence-workflow-workflow-id workflow)
             (length (wave-incidence-workflow-vertices workflow))
             (length (wave-incidence-workflow-edges workflow))
             (wave-incidence-workflow-y-combinator-level workflow)
             (wave-incidence-workflow-execution-strategy workflow)
             (wave-incidence-workflow-spo-modality workflow))))

(defun wave-incidence-execution-context-inspect (context)
  "Inspect execution context"
  (when (wave-incidence-execution-context-p context)
    (message "Incidence Execution Context: %s
  Execution ID: %s
  State: %s
  Current Vertex: %s
  Visited Vertices: %d
  Start Time: %.2f"
             (wave-incidence-execution-context-workflow-id context)
             (wave-incidence-execution-context-execution-id context)
             (wave-incidence-execution-context-state context)
             (wave-incidence-execution-context-current-vertex context)
             (length (wave-incidence-execution-context-visited-vertices context))
             (wave-incidence-execution-context-start-time context))))

;;; Test Functions

(defun wave-incidence-workflow-test-yaml ()
  "Test YAML parsing for incidence workflows"
  (interactive)
  (let ((test-yaml "incidence_workflow:
  id: \"test-incidence-workflow\"
  
  vertices:
    - id: \"v0-read-input\"
      operation: \"read-user-input\"
      type: \"input\"
      
    - id: \"v1-analyze\"
      operation: \"autonomous-analyze-pattern\"
      type: \"eval\"
      
    - id: \"v2-learn\"
      operation: \"autonomous-learn-from-pattern\"
      type: \"eval\"
      
    - id: \"v3-display\"
      operation: \"print-to-buffer\"
      type: \"output\"
      
  edges:
    - id: \"e0\"
      from: \"v0-read-input\"
      to: \"v1-analyze\"
      condition: \"input-valid\"
      
    - id: \"e1\"
      from: \"v1-analyze\"
      to: \"v2-learn\"
      condition: \"pattern-detected\"
      
    - id: \"e2\"
      from: \"v2-learn\"
      to: \"v3-display\"
      condition: \"learning-complete\"
      
  incidence_matrix:
    rows: [\"v0\", \"v1\", \"v2\", \"v3\"]
    cols: [\"e0\", \"e1\", \"e2\"]
    data: [[1,0,0], [0,1,0], [0,0,1], [0,0,0]]
    
  spo_modality:
    subject: \"autonomous-agent\"
    predicate: \"learns-from\"
    object: \"user-input\"
    modality: \"supervised\""))
    
    (let ((workflow (wave-incidence-workflow-parse-yaml test-yaml)))
      (wave-incidence-workflow-inspect workflow)
      (message "Incidence workflow YAML parsing test completed!"))))

(defun wave-incidence-workflow-test-execution ()
  "Test incidence workflow execution"
  (interactive)
  (let ((workflow (wave-incidence-workflow-from-yaml "test-incidence-workflow.yaml")))
    (if workflow
        (progn
          (wave-incidence-workflow-execute workflow)
          (message "Incidence workflow execution test completed!"))
      (message "No test workflow found - create test-incidence-workflow.yaml first"))))

(defun wave-incidence-workflow-test-all ()
  "Test all incidence workflow functionality"
  (interactive)
  (message "Testing incidence workflow system...")
  (wave-incidence-workflow-test-yaml)
  (wave-incidence-workflow-test-execution)
  (message "Incidence workflow tests completed!"))

(provide 'wave-incidence-workflow)

;;; wave-incidence-workflow.el ends here
