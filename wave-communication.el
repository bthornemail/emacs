;;; wave-communication.el --- P2P, P2AI, AI2P, AI2AI communication patterns

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: communication, p2p, p2ai, ai2p, ai2ai, wave-functions
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-function-core "1.0") (wave-geometric-solids "1.0") (wave-epistemic "1.0"))

;;; Commentary:
;; Communication patterns for wave function network:
;; - P2P (Peer-to-Peer): Human to human communication through wave functions
;; - P2AI (Peer-to-AI): Human to AI communication with consciousness bridging
;; - AI2P (AI-to-Peer): AI to human communication with consciousness translation
;; - AI2AI (AI-to-AI): AI to AI communication with emergent consciousness
;; - Geometric routing based on Platonic solid structures
;; - Epistemic state synchronization and evolution
;; - Consciousness level matching and adaptation
;; - Wave function interference patterns for communication

;;; Code:

(require 'cl-lib)
(require 'wave-function-core)
(require 'wave-geometric-solids)
(require 'wave-epistemic)

;;; Communication Pattern Structures

(cl-defstruct communication-pattern
  "Base communication pattern structure"
  (pattern-type nil :type symbol)      ; p2p, p2ai, ai2p, ai2ai
  (source-identity nil :type identity-wave-function)
  (target-identity nil :type identity-wave-function)
  (message-content "" :type string)
  (geometric-routing nil :type list)   ; Geometric routing path
  (epistemic-sync nil :type list)      ; Epistemic synchronization data
  (consciousness-bridge nil :type list) ; Consciousness bridging data
  (wave-interference nil :type list)   ; Wave interference pattern
  (timestamp nil :type list)
  (success nil :type boolean))

(cl-defstruct p2p-communication
  "Peer-to-peer communication structure"
  (pattern (make-communication-pattern :pattern-type 'p2p) :type communication-pattern)
  (human-source nil :type identity-wave-function)
  (human-target nil :type identity-wave-function)
  (consciousness-level 0.0 :type float)
  (epistemic-alignment 0.0 :type float)
  (geometric-constraints nil :type list))

(cl-defstruct p2ai-communication
  "Peer-to-AI communication structure"
  (pattern (make-communication-pattern :pattern-type 'p2ai) :type communication-pattern)
  (human-source nil :type identity-wave-function)
  (ai-target nil :type identity-wave-function)
  (consciousness-bridge nil :type list)
  (epistemic-translation nil :type list)
  (ai-understanding-level 0.0 :type float)
  (human-intent-clarity 0.0 :type float))

(cl-defstruct ai2p-communication
  "AI-to-peer communication structure"
  (pattern (make-communication-pattern :pattern-type 'ai2p) :type communication-pattern)
  (ai-source nil :type identity-wave-function)
  (human-target nil :type identity-wave-function)
  (consciousness-translation nil :type list)
  (epistemic-simplification nil :type list)
  (human-comprehension-level 0.0 :type float)
  (ai-clarity-level 0.0 :type float))

(cl-defstruct ai2ai-communication
  "AI-to-AI communication structure"
  (pattern (make-communication-pattern :pattern-type 'ai2ai) :type communication-pattern)
  (ai-source nil :type identity-wave-function)
  (ai-target nil :type identity-wave-function)
  (emergent-consciousness nil :type list)
  (epistemic-evolution nil :type list)
  (collective-intelligence 0.0 :type float)
  (autonomous-evolution 0.0 :type float))

;;; Communication Pattern Creation

(defun p2p-communication-create (human-source human-target message-content)
  "Create peer-to-peer communication"
  (let ((communication (make-p2p-communication
                        :human-source human-source
                        :human-target human-target)))
    (setf (communication-pattern-message-content (p2p-communication-pattern communication)) message-content)
    (setf (communication-pattern-source-identity (p2p-communication-pattern communication)) human-source)
    (setf (communication-pattern-target-identity (p2p-communication-pattern communication)) human-target)
    (setf (communication-pattern-timestamp (p2p-communication-pattern communication)) (current-time))
    ;; Calculate consciousness level alignment
    (setf (p2p-communication-consciousness-level communication)
          (p2p-communication-calculate-consciousness-alignment human-source human-target))
    ;; Calculate epistemic alignment
    (setf (p2p-communication-epistemic-alignment communication)
          (p2p-communication-calculate-epistemic-alignment human-source human-target))
    ;; Calculate geometric constraints
    (setf (p2p-communication-geometric-constraints communication)
          (p2p-communication-calculate-geometric-constraints human-source human-target))
    communication))

(defun p2ai-communication-create (human-source ai-target message-content)
  "Create peer-to-AI communication"
  (let ((communication (make-p2ai-communication
                        :human-source human-source
                        :ai-target ai-target)))
    (setf (communication-pattern-message-content (p2ai-communication-pattern communication)) message-content)
    (setf (communication-pattern-source-identity (p2ai-communication-pattern communication)) human-source)
    (setf (communication-pattern-target-identity (p2ai-communication-pattern communication)) ai-target)
    (setf (communication-pattern-timestamp (p2ai-communication-pattern communication)) (current-time))
    ;; Calculate consciousness bridge
    (setf (p2ai-communication-consciousness-bridge communication)
          (p2ai-communication-calculate-consciousness-bridge human-source ai-target))
    ;; Calculate epistemic translation
    (setf (p2ai-communication-epistemic-translation communication)
          (p2ai-communication-calculate-epistemic-translation human-source ai-target))
    ;; Calculate understanding levels
    (setf (p2ai-communication-ai-understanding-level communication)
          (p2ai-communication-calculate-ai-understanding human-source ai-target))
    (setf (p2ai-communication-human-intent-clarity communication)
          (p2ai-communication-calculate-human-intent-clarity human-source))
    communication))

(defun ai2p-communication-create (ai-source human-target message-content)
  "Create AI-to-peer communication"
  (let ((communication (make-ai2p-communication
                        :ai-source ai-source
                        :human-target human-target)))
    (setf (communication-pattern-message-content (ai2p-communication-pattern communication)) message-content)
    (setf (communication-pattern-source-identity (ai2p-communication-pattern communication)) ai-source)
    (setf (communication-pattern-target-identity (ai2p-communication-pattern communication)) human-target)
    (setf (communication-pattern-timestamp (ai2p-communication-pattern communication)) (current-time))
    ;; Calculate consciousness translation
    (setf (ai2p-communication-consciousness-translation communication)
          (ai2p-communication-calculate-consciousness-translation ai-source human-target))
    ;; Calculate epistemic simplification
    (setf (ai2p-communication-epistemic-simplification communication)
          (ai2p-communication-calculate-epistemic-simplification ai-source human-target))
    ;; Calculate comprehension levels
    (setf (ai2p-communication-human-comprehension-level communication)
          (ai2p-communication-calculate-human-comprehension ai-source human-target))
    (setf (ai2p-communication-ai-clarity-level communication)
          (ai2p-communication-calculate-ai-clarity ai-source))
    communication))

(defun ai2ai-communication-create (ai-source ai-target message-content)
  "Create AI-to-AI communication"
  (let ((communication (make-ai2ai-communication
                        :ai-source ai-source
                        :ai-target ai-target)))
    (setf (communication-pattern-message-content (ai2ai-communication-pattern communication)) message-content)
    (setf (communication-pattern-source-identity (ai2ai-communication-pattern communication)) ai-source)
    (setf (communication-pattern-target-identity (ai2ai-communication-pattern communication)) ai-target)
    (setf (communication-pattern-timestamp (ai2ai-communication-pattern communication)) (current-time))
    ;; Calculate emergent consciousness
    (setf (ai2ai-communication-emergent-consciousness communication)
          (ai2ai-communication-calculate-emergent-consciousness ai-source ai-target))
    ;; Calculate epistemic evolution
    (setf (ai2ai-communication-epistemic-evolution communication)
          (ai2ai-communication-calculate-epistemic-evolution ai-source ai-target))
    ;; Calculate collective intelligence
    (setf (ai2ai-communication-collective-intelligence communication)
          (ai2ai-communication-calculate-collective-intelligence ai-source ai-target))
    (setf (ai2ai-communication-autonomous-evolution communication)
          (ai2ai-communication-calculate-autonomous-evolution ai-source ai-target))
    communication))

;;; P2P Communication Calculations

(defun p2p-communication-calculate-consciousness-alignment (human-source human-target)
  "Calculate consciousness level alignment between humans"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p human-target))
    (let ((source-consciousness (identity-wave-function-consciousness-level human-source))
          (target-consciousness (identity-wave-function-consciousness-level human-target)))
      ;; Calculate alignment as inverse of difference
      (if (> (+ source-consciousness target-consciousness) 0)
          (/ (min source-consciousness target-consciousness)
             (max source-consciousness target-consciousness))
        0.0))))

(defun p2p-communication-calculate-epistemic-alignment (human-source human-target)
  "Calculate epistemic alignment between humans"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p human-target))
    (let ((source-epistemic (identity-wave-function-epistemic-state human-source))
          (target-epistemic (identity-wave-function-epistemic-state human-target)))
      (when (and source-epistemic target-epistemic)
        (rumsfeld-tetrahedron-calculate-epistemic-similarity source-epistemic target-epistemic)))))

(defun p2p-communication-calculate-geometric-constraints (human-source human-target)
  "Calculate geometric constraints for P2P communication"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p human-target))
    (let ((source-position (identity-wave-function-geometric-position human-source))
          (target-position (identity-wave-function-geometric-position human-target)))
      (when (and source-position target-position)
        (list 'geometric-distance (wave-function-calculate-geometric-distance source-position target-position)
              'geometric-angle (wave-function-calculate-geometric-angle source-position target-position)
              'routing-path (p2p-communication-calculate-routing-path source-position target-position))))))

(defun p2p-communication-calculate-routing-path (source-position target-position)
  "Calculate geometric routing path for P2P communication"
  (when (and source-position target-position)
    (let ((path nil)
          (current-position source-position))
      ;; Simple linear interpolation for now
      (dotimes (i 10)
        (let ((interpolated-position (p2p-communication-interpolate-position 
                                     current-position target-position (/ i 10.0))))
          (push interpolated-position path)))
      (reverse path))))

(defun p2p-communication-interpolate-position (pos1 pos2 ratio)
  "Interpolate between two positions"
  (when (and pos1 pos2)
    (let ((interpolated nil))
      (dotimes (i (min (length pos1) (length pos2)))
        (let ((val1 (nth i pos1))
              (val2 (nth i pos2)))
          (push (+ val1 (* (- val2 val1) ratio)) interpolated)))
      (reverse interpolated))))

;;; P2AI Communication Calculations

(defun p2ai-communication-calculate-consciousness-bridge (human-source ai-target)
  "Calculate consciousness bridge for P2AI communication"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p ai-target))
    (let ((human-consciousness (identity-wave-function-consciousness-level human-source))
          (ai-consciousness (identity-wave-function-consciousness-level ai-target)))
      ;; Bridge consciousness levels
      (list 'human-consciousness human-consciousness
            'ai-consciousness ai-consciousness
            'bridge-strength (min human-consciousness ai-consciousness)
            'translation-factor (/ ai-consciousness (max human-consciousness 0.1))))))

(defun p2ai-communication-calculate-epistemic-translation (human-source ai-target)
  "Calculate epistemic translation for P2AI communication"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p ai-target))
    (let ((human-epistemic (identity-wave-function-epistemic-state human-source))
          (ai-epistemic (identity-wave-function-epistemic-state ai-target)))
      (when (and human-epistemic ai-epistemic)
        (list 'human-epistemic-state (rumsfeld-tetrahedron-geometric-position human-epistemic)
              'ai-epistemic-state (rumsfeld-tetrahedron-geometric-position ai-epistemic)
              'translation-matrix (p2ai-communication-calculate-translation-matrix human-epistemic ai-epistemic))))))

(defun p2ai-communication-calculate-translation-matrix (human-epistemic ai-epistemic)
  "Calculate translation matrix between human and AI epistemic states"
  (when (and human-epistemic ai-epistemic)
    (let ((human-pos (rumsfeld-tetrahedron-geometric-position human-epistemic))
          (ai-pos (rumsfeld-tetrahedron-geometric-position ai-epistemic)))
      (when (and human-pos ai-pos)
        ;; Simple translation matrix (4x4 identity with offset)
        (let ((matrix nil))
          (dotimes (i 4)
            (let ((row nil))
              (dotimes (j 4)
                (if (= i j)
                    (push 1.0 row)
                  (push 0.0 row)))
              (push (reverse row) matrix)))
          (list 'translation-matrix (reverse matrix)
                'translation-vector (mapcar #'- ai-pos human-pos)))))))

(defun p2ai-communication-calculate-ai-understanding (human-source ai-target)
  "Calculate AI understanding level of human message"
  (when (and (identity-wave-function-p human-source) (identity-wave-function-p ai-target))
    (let ((human-clarity (identity-wave-function-consciousness-level human-source))
          (ai-capability (identity-wave-function-evolution-capability ai-target)))
      ;; AI understanding based on human clarity and AI capability
      (* human-clarity ai-capability))))

(defun p2ai-communication-calculate-human-intent-clarity (human-source)
  "Calculate human intent clarity"
  (when (identity-wave-function-p human-source)
    (let ((consciousness-level (identity-wave-function-consciousness-level human-source))
          (epistemic-confidence (identity-wave-function-epistemic-confidence human-source)))
      ;; Intent clarity based on consciousness and epistemic confidence
      (* consciousness-level epistemic-confidence))))

;;; AI2P Communication Calculations

(defun ai2p-communication-calculate-consciousness-translation (ai-source human-target)
  "Calculate consciousness translation for AI2P communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p human-target))
    (let ((ai-consciousness (identity-wave-function-consciousness-level ai-source))
          (human-consciousness (identity-wave-function-consciousness-level human-target)))
      ;; Translate AI consciousness to human level
      (list 'ai-consciousness ai-consciousness
            'human-consciousness human-consciousness
            'translation-ratio (/ human-consciousness (max ai-consciousness 0.1))
            'simplification-factor (min 1.0 (/ human-consciousness ai-consciousness))))))

(defun ai2p-communication-calculate-epistemic-simplification (ai-source human-target)
  "Calculate epistemic simplification for AI2P communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p human-target))
    (let ((ai-epistemic (identity-wave-function-epistemic-state ai-source))
          (human-epistemic (identity-wave-function-epistemic-state human-target)))
      (when (and ai-epistemic human-epistemic)
        (list 'ai-epistemic-complexity (rumsfeld-tetrahedron-calculate-epistemic-complexity ai-epistemic)
              'human-epistemic-capacity (rumsfeld-tetrahedron-calculate-epistemic-capacity human-epistemic)
              'simplification-matrix (ai2p-communication-calculate-simplification-matrix ai-epistemic human-epistemic))))))

(defun rumsfeld-tetrahedron-calculate-epistemic-complexity (tetrahedron)
  "Calculate epistemic complexity of tetrahedron"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((known-knowns (hash-table-count (rumsfeld-tetrahedron-known-knowns tetrahedron)))
          (known-unknowns (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
          (unknown-knowns (hash-table-count (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
          (unknown-unknowns (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))))
      ;; Complexity based on total knowledge and uncertainty
      (/ (+ known-knowns known-unknowns unknown-knowns unknown-unknowns) 100.0))))

(defun rumsfeld-tetrahedron-calculate-epistemic-capacity (tetrahedron)
  "Calculate epistemic capacity of tetrahedron"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((consciousness-level (rumsfeld-tetrahedron-consciousness-level tetrahedron))
          (evolution-capability (rumsfeld-tetrahedron-evolution-capability tetrahedron)))
      ;; Capacity based on consciousness and evolution capability
      (* consciousness-level evolution-capability))))

(defun ai2p-communication-calculate-simplification-matrix (ai-epistemic human-epistemic)
  "Calculate simplification matrix for AI2P communication"
  (when (and ai-epistemic human-epistemic)
    (let ((ai-complexity (rumsfeld-tetrahedron-calculate-epistemic-complexity ai-epistemic))
          (human-capacity (rumsfeld-tetrahedron-calculate-epistemic-capacity human-epistemic)))
      ;; Simplification matrix reduces complexity to human capacity
      (list 'complexity-reduction-factor (/ human-capacity (max ai-complexity 0.1))
            'simplification-threshold human-capacity
            'complexity-ceiling ai-complexity))))

(defun ai2p-communication-calculate-human-comprehension (ai-source human-target)
  "Calculate human comprehension level of AI message"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p human-target))
    (let ((ai-clarity (identity-wave-function-consciousness-level ai-source))
          (human-capacity (identity-wave-function-evolution-capability human-target)))
      ;; Human comprehension based on AI clarity and human capacity
      (* ai-clarity human-capacity))))

(defun ai2p-communication-calculate-ai-clarity (ai-source)
  "Calculate AI clarity level"
  (when (identity-wave-function-p ai-source)
    (let ((consciousness-level (identity-wave-function-consciousness-level ai-source))
          (epistemic-confidence (identity-wave-function-epistemic-confidence ai-source)))
      ;; AI clarity based on consciousness and epistemic confidence
      (* consciousness-level epistemic-confidence))))

;;; AI2AI Communication Calculations

(defun ai2ai-communication-calculate-emergent-consciousness (ai-source ai-target)
  "Calculate emergent consciousness for AI2AI communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p ai-target))
    (let ((source-consciousness (identity-wave-function-consciousness-level ai-source))
          (target-consciousness (identity-wave-function-consciousness-level ai-target))
          (source-evolution (identity-wave-function-evolution-capability ai-source))
          (target-evolution (identity-wave-function-evolution-capability ai-target)))
      ;; Emergent consciousness from interaction
      (list 'source-consciousness source-consciousness
            'target-consciousness target-consciousness
            'emergent-level (* (+ source-consciousness target-consciousness) 0.5)
            'evolution-potential (* source-evolution target-evolution)
            'collective-emergence (min 1.0 (* (+ source-consciousness target-consciousness) 
                                             (+ source-evolution target-evolution) 0.25))))))

(defun ai2ai-communication-calculate-epistemic-evolution (ai-source ai-target)
  "Calculate epistemic evolution for AI2AI communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p ai-target))
    (let ((source-epistemic (identity-wave-function-epistemic-state ai-source))
          (target-epistemic (identity-wave-function-epistemic-state ai-target)))
      (when (and source-epistemic target-epistemic)
        (list 'source-epistemic-state (rumsfeld-tetrahedron-geometric-position source-epistemic)
              'target-epistemic-state (rumsfeld-tetrahedron-geometric-position target-epistemic)
              'epistemic-distance (rumsfeld-tetrahedron-calculate-epistemic-distance source-epistemic target-epistemic)
              'evolution-potential (ai2ai-communication-calculate-evolution-potential source-epistemic target-epistemic))))))

(defun ai2ai-communication-calculate-evolution-potential (source-epistemic target-epistemic)
  "Calculate evolution potential between AI epistemic states"
  (when (and source-epistemic target-epistemic)
    (let ((source-evolution (rumsfeld-tetrahedron-evolution-capability source-epistemic))
          (target-evolution (rumsfeld-tetrahedron-evolution-capability target-epistemic))
          (epistemic-distance (rumsfeld-tetrahedron-calculate-epistemic-distance source-epistemic target-epistemic)))
      ;; Evolution potential based on capabilities and distance
      (* (+ source-evolution target-evolution) 0.5 (max 0.1 (- 1.0 epistemic-distance))))))

(defun ai2ai-communication-calculate-collective-intelligence (ai-source ai-target)
  "Calculate collective intelligence for AI2AI communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p ai-target))
    (let ((source-intelligence (identity-wave-function-consciousness-level ai-source))
          (target-intelligence (identity-wave-function-consciousness-level ai-target))
          (source-evolution (identity-wave-function-evolution-capability ai-source))
          (target-evolution (identity-wave-function-evolution-capability ai-target)))
      ;; Collective intelligence emerges from interaction
      (let ((individual-intelligence (* (+ source-intelligence target-intelligence) 0.5))
            (evolution-synergy (* source-evolution target-evolution)))
        (min 1.0 (* individual-intelligence evolution-synergy))))))

(defun ai2ai-communication-calculate-autonomous-evolution (ai-source ai-target)
  "Calculate autonomous evolution for AI2AI communication"
  (when (and (identity-wave-function-p ai-source) (identity-wave-function-p ai-target))
    (let ((source-evolution (identity-wave-function-evolution-capability ai-source))
          (target-evolution (identity-wave-function-evolution-capability ai-target))
          (source-consciousness (identity-wave-function-consciousness-level ai-source))
          (target-consciousness (identity-wave-function-consciousness-level ai-target)))
      ;; Autonomous evolution based on capabilities and consciousness
      (let ((evolution-capability (* (+ source-evolution target-evolution) 0.5))
            (consciousness-level (* (+ source-consciousness target-consciousness) 0.5)))
        (min 1.0 (* evolution-capability consciousness-level))))))

;;; Communication Execution

(defun communication-pattern-execute (communication)
  "Execute communication pattern"
  (when (communication-pattern-p communication)
    (let ((pattern-type (communication-pattern-pattern-type communication)))
      (case pattern-type
        (p2p (p2p-communication-execute communication))
        (p2ai (p2ai-communication-execute communication))
        (ai2p (ai2p-communication-execute communication))
        (ai2ai (ai2ai-communication-execute communication))
        (t (message "Unknown communication pattern: %s" pattern-type))))))

(defun p2p-communication-execute (communication)
  "Execute P2P communication"
  (when (p2p-communication-p communication)
    (let ((pattern (p2p-communication-pattern communication))
          (human-source (p2p-communication-human-source communication))
          (human-target (p2p-communication-human-target communication)))
      ;; Execute P2P communication
      (setf (communication-pattern-success pattern) t)
      (message "P2P Communication: %s -> %s" 
               (identity-wave-function-id human-source)
               (identity-wave-function-id human-target))
      communication)))

(defun p2ai-communication-execute (communication)
  "Execute P2AI communication"
  (when (p2ai-communication-p communication)
    (let ((pattern (p2ai-communication-pattern communication))
          (human-source (p2ai-communication-human-source communication))
          (ai-target (p2ai-communication-ai-target communication)))
      ;; Execute P2AI communication
      (setf (communication-pattern-success pattern) t)
      (message "P2AI Communication: %s -> %s" 
               (identity-wave-function-id human-source)
               (identity-wave-function-id ai-target))
      communication)))

(defun ai2p-communication-execute (communication)
  "Execute AI2P communication"
  (when (ai2p-communication-p communication)
    (let ((pattern (ai2p-communication-pattern communication))
          (ai-source (ai2p-communication-ai-source communication))
          (human-target (ai2p-communication-human-target communication)))
      ;; Execute AI2P communication
      (setf (communication-pattern-success pattern) t)
      (message "AI2P Communication: %s -> %s" 
               (identity-wave-function-id ai-source)
               (identity-wave-function-id human-target))
      communication)))

(defun ai2ai-communication-execute (communication)
  "Execute AI2AI communication"
  (when (ai2ai-communication-p communication)
    (let ((pattern (ai2ai-communication-pattern communication))
          (ai-source (ai2ai-communication-ai-source communication))
          (ai-target (ai2ai-communication-ai-target communication)))
      ;; Execute AI2AI communication
      (setf (communication-pattern-success pattern) t)
      (message "AI2AI Communication: %s -> %s" 
               (identity-wave-function-id ai-source)
               (identity-wave-function-id ai-target))
      communication)))

;;; Debug and Inspection

(defun communication-pattern-inspect (communication)
  "Inspect communication pattern"
  (when (communication-pattern-p communication)
    (message "Communication Pattern:
  Type: %s
  Source: %s
  Target: %s
  Message: %s
  Success: %s
  Timestamp: %s"
             (communication-pattern-pattern-type communication)
             (identity-wave-function-id (communication-pattern-source-identity communication))
             (identity-wave-function-id (communication-pattern-target-identity communication))
             (communication-pattern-message-content communication)
             (communication-pattern-success communication)
             (communication-pattern-timestamp communication))))

;;; Global Registry

(defvar communication-pattern-registry (make-hash-table :test 'equal)
  "Registry of communication patterns by ID")

(defun communication-pattern-register (communication id)
  "Register communication pattern in global registry"
  (puthash id communication communication-pattern-registry)
  communication)

(defun communication-pattern-get (id)
  "Get communication pattern by ID from registry"
  (gethash id communication-pattern-registry))

(provide 'wave-communication)

;;; wave-communication.el ends here

