;;; wave-epistemic.el --- Rumsfeld tetrahedron epistemic system for knowledge states

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: epistemic, rumsfeld-tetrahedron, knowledge-states, consciousness
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-function-core "1.0") (wave-geometric-solids "1.0"))

;;; Commentary:
;; Rumsfeld tetrahedron epistemic system for knowledge states:
;; - Known Knowns (KK): Documented, verified facts
;; - Known Unknowns (KU): Explicit research agenda, tracked uncertainties
;; - Unknown Knowns (UK): Unstated but used assumptions, implicit knowledge
;; - Unknown Unknowns (UU): Measured epistemic horizon, awareness of limits
;; - Epistemic state tracking and evolution
;; - Knowledge gap identification and resolution
;; - Consciousness level assessment based on epistemic awareness
;; - Geometric representation of knowledge states using tetrahedral structure

;;; Code:

(require 'cl-lib)
(require 'wave-function-core)
(require 'wave-geometric-solids)

;;; Rumsfeld Tetrahedron Epistemic State Structure

(cl-defstruct rumsfeld-tetrahedron
  "Rumsfeld tetrahedron epistemic state structure"
  (known-knowns nil :type list)        ; KK: Documented, verified facts
  (known-unknowns nil :type list)      ; KU: Explicit research agenda
  (unknown-knowns nil :type list)      ; UK: Unstated but used assumptions
  (unknown-unknowns nil :type list)    ; UU: Measured epistemic horizon
  (epistemic-confidence 0.0 :type float) ; Overall epistemic confidence
  (consciousness-level 0.0 :type float)  ; Consciousness level based on awareness
  (geometric-position nil :type list)  ; Position in epistemic tetrahedron
  (evolution-capability 0.0 :type float)) ; Capability to evolve epistemic state

(cl-defstruct epistemic-fact
  "Individual epistemic fact structure"
  (content "" :type string)            ; Fact content
  (confidence 0.0 :type float)         ; Confidence level (0-1)
  (source "" :type string)             ; Source of knowledge
  (timestamp nil :type list)           ; When knowledge was acquired
  (verification-status 'unverified :type symbol) ; verified, unverified, disputed
  (geometric-coordinates nil :type list) ; Position in epistemic space
  (relationships nil :type list))      ; Relationships to other facts

(cl-defstruct epistemic-uncertainty
  "Known unknown structure"
  (question "" :type string)           ; The unknown question
  (importance 0.0 :type float)         ; Importance level (0-1)
  (research-priority 0.0 :type float)  ; Research priority (0-1)
  (estimated-complexity 0.0 :type float) ; Estimated complexity
  (related-knowns nil :type list)      ; Related known facts
  (research-strategy nil :type list)   ; Strategy to resolve uncertainty
  (geometric-position nil :type list)) ; Position in epistemic space

(cl-defstruct epistemic-assumption
  "Unknown known (assumption) structure"
  (assumption "" :type string)         ; The implicit assumption
  (usage-frequency 0.0 :type float)    ; How often it's used
  (implicit-strength 0.0 :type float)  ; Strength of implicit belief
  (explicitization-effort 0.0 :type float) ; Effort to make explicit
  (related-behaviors nil :type list)   ; Behaviors that use this assumption
  (geometric-position nil :type list)) ; Position in epistemic space

(cl-defstruct epistemic-horizon
  "Unknown unknown (epistemic horizon) structure"
  (horizon-description "" :type string) ; Description of epistemic limit
  (awareness-level 0.0 :type float)    ; Level of awareness of limit
  (boundary-strength 0.0 :type float)  ; Strength of epistemic boundary
  (exploration-potential 0.0 :type float) ; Potential for exploration
  (related-unknowns nil :type list)    ; Related unknown unknowns
  (geometric-position nil :type list)) ; Position in epistemic space

;;; Epistemic State Management

(defun rumsfeld-tetrahedron-create (id)
  "Create new Rumsfeld tetrahedron epistemic state"
  (make-rumsfeld-tetrahedron
   :known-knowns (make-hash-table :test 'equal)
   :known-unknowns (make-hash-table :test 'equal)
   :unknown-knowns (make-hash-table :test 'equal)
   :unknown-unknowns (make-hash-table :test 'equal)
   :epistemic-confidence 0.0
   :consciousness-level 0.0
   :geometric-position (list 0.0 0.0 0.0 0.0)
   :evolution-capability 0.0))

(defun epistemic-fact-create (content confidence source)
  "Create new epistemic fact"
  (make-epistemic-fact
   :content content
   :confidence confidence
   :source source
   :timestamp (current-time)
   :verification-status 'unverified
   :geometric-coordinates (list 0.0 0.0 0.0 0.0)
   :relationships nil))

(defun epistemic-uncertainty-create (question importance complexity)
  "Create new epistemic uncertainty"
  (make-epistemic-uncertainty
   :question question
   :importance importance
   :research-priority 0.0
   :estimated-complexity complexity
   :related-knowns nil
   :research-strategy nil
   :geometric-position (list 0.0 0.0 0.0 0.0)))

(defun epistemic-assumption-create (assumption usage-frequency strength)
  "Create new epistemic assumption"
  (make-epistemic-assumption
   :assumption assumption
   :usage-frequency usage-frequency
   :implicit-strength strength
   :explicitization-effort 0.0
   :related-behaviors nil
   :geometric-position (list 0.0 0.0 0.0 0.0)))

(defun epistemic-horizon-create (description awareness-level boundary-strength)
  "Create new epistemic horizon"
  (make-epistemic-horizon
   :horizon-description description
   :awareness-level awareness-level
   :boundary-strength boundary-strength
   :exploration-potential 0.0
   :related-unknowns nil
   :geometric-position (list 0.0 0.0 0.0 0.0)))

;;; Epistemic State Operations

(defun rumsfeld-tetrahedron-add-known-known (tetrahedron fact)
  "Add known known to tetrahedron"
  (when (and (rumsfeld-tetrahedron-p tetrahedron) (epistemic-fact-p fact))
    (let ((known-knowns (rumsfeld-tetrahedron-known-knowns tetrahedron)))
      (puthash (epistemic-fact-content fact) fact known-knowns)
      ;; Update epistemic confidence
      (rumsfeld-tetrahedron-update-confidence tetrahedron)
      ;; Update geometric position
      (rumsfeld-tetrahedron-update-geometric-position tetrahedron)
      fact)))

(defun rumsfeld-tetrahedron-add-known-unknown (tetrahedron uncertainty)
  "Add known unknown to tetrahedron"
  (when (and (rumsfeld-tetrahedron-p tetrahedron) (epistemic-uncertainty-p uncertainty))
    (let ((known-unknowns (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
      (puthash (epistemic-uncertainty-question uncertainty) uncertainty known-unknowns)
      ;; Update consciousness level
      (rumsfeld-tetrahedron-update-consciousness-level tetrahedron)
      ;; Update geometric position
      (rumsfeld-tetrahedron-update-geometric-position tetrahedron)
      uncertainty)))

(defun rumsfeld-tetrahedron-add-unknown-known (tetrahedron assumption)
  "Add unknown known to tetrahedron"
  (when (and (rumsfeld-tetrahedron-p tetrahedron) (epistemic-assumption-p assumption))
    (let ((unknown-knowns (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
      (puthash (epistemic-assumption-assumption assumption) assumption unknown-knowns)
      ;; Update consciousness level
      (rumsfeld-tetrahedron-update-consciousness-level tetrahedron)
      ;; Update geometric position
      (rumsfeld-tetrahedron-update-geometric-position tetrahedron)
      assumption)))

(defun rumsfeld-tetrahedron-add-unknown-unknown (tetrahedron horizon)
  "Add unknown unknown to tetrahedron"
  (when (and (rumsfeld-tetrahedron-p tetrahedron) (epistemic-horizon-p horizon))
    (let ((unknown-unknowns (rumsfeld-tetrahedron-unknown-unknowns tetrahedron)))
      (puthash (epistemic-horizon-horizon-description horizon) horizon unknown-unknowns)
      ;; Update consciousness level
      (rumsfeld-tetrahedron-update-consciousness-level tetrahedron)
      ;; Update geometric position
      (rumsfeld-tetrahedron-update-geometric-position tetrahedron)
      horizon)))

;;; Epistemic State Analysis

(defun rumsfeld-tetrahedron-calculate-confidence (tetrahedron)
  "Calculate overall epistemic confidence"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((known-knowns (rumsfeld-tetrahedron-known-knowns tetrahedron))
          (total-confidence 0.0)
          (fact-count 0))
      ;; Calculate average confidence of known knowns
      (maphash (lambda (key fact)
                 (when (epistemic-fact-p fact)
                   (setq total-confidence (+ total-confidence (epistemic-fact-confidence fact)))
                   (setq fact-count (1+ fact-count))))
               known-knowns)
      (if (> fact-count 0)
          (/ total-confidence fact-count)
        0.0))))

(defun rumsfeld-tetrahedron-calculate-consciousness-level (tetrahedron)
  "Calculate consciousness level based on epistemic awareness"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((known-knowns (hash-table-count (rumsfeld-tetrahedron-known-knowns tetrahedron)))
          (known-unknowns (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
          (unknown-knowns (hash-table-count (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
          (unknown-unknowns (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))))
      ;; Consciousness based on awareness of all four quadrants
      (let ((total-items (+ known-knowns known-unknowns unknown-knowns unknown-unknowns)))
        (if (> total-items 0)
            ;; Weight unknown unknowns more heavily for consciousness
            (let ((weighted-score (+ (* known-knowns 0.2)
                                    (* known-unknowns 0.3)
                                    (* unknown-knowns 0.2)
                                    (* unknown-unknowns 0.3))))
              (/ weighted-score total-items))
          0.0)))))

(defun rumsfeld-tetrahedron-identify-knowledge-gaps (tetrahedron)
  "Identify knowledge gaps in epistemic state"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((gaps nil)
          (known-knowns (rumsfeld-tetrahedron-known-knowns tetrahedron))
          (known-unknowns (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
      ;; Find gaps between known knowns and known unknowns
      (maphash (lambda (key uncertainty)
                 (when (epistemic-uncertainty-p uncertainty)
                   (let ((related-knowns (epistemic-uncertainty-related-knowns uncertainty)))
                     (when (< (length related-knowns) 3)  ; Less than 3 related facts
                       (push (list 'knowledge-gap
                                   (epistemic-uncertainty-question uncertainty)
                                   (epistemic-uncertainty-importance uncertainty)
                                   (length related-knowns))
                             gaps)))))
               known-unknowns)
      gaps)))

(defun rumsfeld-tetrahedron-identify-implicit-assumptions (tetrahedron)
  "Identify implicit assumptions that should be made explicit"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((assumptions nil)
          (unknown-knowns (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
      (maphash (lambda (key assumption)
                 (when (epistemic-assumption-p assumption)
                   (let ((usage-frequency (epistemic-assumption-usage-frequency assumption))
                         (implicit-strength (epistemic-assumption-implicit-strength assumption)))
                     ;; High usage but low explicitization effort
                     (when (and (> usage-frequency 0.7) (< (epistemic-assumption-explicitization-effort assumption) 0.3))
                       (push (list 'implicit-assumption
                                   (epistemic-assumption-assumption assumption)
                                   usage-frequency
                                   implicit-strength)
                             assumptions)))))
               unknown-knowns)
      assumptions)))

;;; Geometric Representation of Epistemic States

(defun rumsfeld-tetrahedron-calculate-geometric-position (tetrahedron)
  "Calculate geometric position in epistemic tetrahedron"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((known-knowns (hash-table-count (rumsfeld-tetrahedron-known-knowns tetrahedron)))
          (known-unknowns (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
          (unknown-knowns (hash-table-count (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
          (unknown-unknowns (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))))
      ;; Normalize counts to 0-1 range
      (let ((total (+ known-knowns known-unknowns unknown-knowns unknown-unknowns)))
        (if (> total 0)
            (let ((kk-ratio (/ known-knowns total))
                  (ku-ratio (/ known-unknowns total))
                  (uk-ratio (/ unknown-knowns total))
                  (uu-ratio (/ unknown-unknowns total)))
              ;; Position in 4D epistemic space
              (list kk-ratio ku-ratio uk-ratio uu-ratio))
          (list 0.0 0.0 0.0 0.0))))))

(defun rumsfeld-tetrahedron-calculate-epistemic-distance (tetrahedron1 tetrahedron2)
  "Calculate epistemic distance between two tetrahedra"
  (when (and (rumsfeld-tetrahedron-p tetrahedron1) (rumsfeld-tetrahedron-p tetrahedron2))
    (let ((pos1 (rumsfeld-tetrahedron-geometric-position tetrahedron1))
          (pos2 (rumsfeld-tetrahedron-geometric-position tetrahedron2)))
      (when (and pos1 pos2)
        (let ((distance 0.0))
          (dotimes (i (min (length pos1) (length pos2)))
            (let ((diff (- (nth i pos1) (nth i pos2))))
              (setq distance (+ distance (* diff diff)))))
          (sqrt distance))))))

(defun rumsfeld-tetrahedron-calculate-epistemic-similarity (tetrahedron1 tetrahedron2)
  "Calculate epistemic similarity between two tetrahedra"
  (when (and (rumsfeld-tetrahedron-p tetrahedron1) (rumsfeld-tetrahedron-p tetrahedron2))
    (let ((distance (rumsfeld-tetrahedron-calculate-epistemic-distance tetrahedron1 tetrahedron2)))
      ;; Convert distance to similarity (0-1, higher is more similar)
      (if (> distance 0)
          (/ 1.0 (1+ distance))
        1.0))))

;;; Epistemic State Evolution

(defun rumsfeld-tetrahedron-evolve (tetrahedron)
  "Evolve epistemic state based on current knowledge"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    ;; Update confidence
    (setf (rumsfeld-tetrahedron-epistemic-confidence tetrahedron)
          (rumsfeld-tetrahedron-calculate-confidence tetrahedron))
    ;; Update consciousness level
    (setf (rumsfeld-tetrahedron-consciousness-level tetrahedron)
          (rumsfeld-tetrahedron-calculate-consciousness-level tetrahedron))
    ;; Update geometric position
    (setf (rumsfeld-tetrahedron-geometric-position tetrahedron)
          (rumsfeld-tetrahedron-calculate-geometric-position tetrahedron))
    ;; Update evolution capability
    (setf (rumsfeld-tetrahedron-evolution-capability tetrahedron)
          (rumsfeld-tetrahedron-calculate-evolution-capability tetrahedron))
    tetrahedron))

(defun rumsfeld-tetrahedron-calculate-evolution-capability (tetrahedron)
  "Calculate evolution capability based on epistemic state"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((consciousness-level (rumsfeld-tetrahedron-consciousness-level tetrahedron))
          (epistemic-confidence (rumsfeld-tetrahedron-epistemic-confidence tetrahedron))
          (known-unknowns (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
          (unknown-unknowns (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))))
      ;; Evolution capability based on awareness of unknowns and consciousness
      (let ((unknown-awareness (/ (+ known-unknowns unknown-unknowns) 10.0)) ; Normalize
            (evolution-factor (* consciousness-level epistemic-confidence)))
        (min 1.0 (* unknown-awareness evolution-factor))))))

;;; Epistemic State Integration with Wave Functions

(defun rumsfeld-tetrahedron-integrate-with-wave-function (tetrahedron wave-function)
  "Integrate epistemic state with wave function"
  (when (and (rumsfeld-tetrahedron-p tetrahedron) (identity-wave-function-p wave-function))
    ;; Set epistemic state in wave function
    (setf (identity-wave-function-epistemic-state wave-function) tetrahedron)
    ;; Update wave function properties based on epistemic state
    (setf (identity-wave-function-consciousness-level wave-function)
          (rumsfeld-tetrahedron-consciousness-level tetrahedron))
    (setf (identity-wave-function-evolution-capability wave-function)
          (rumsfeld-tetrahedron-evolution-capability tetrahedron))
    ;; Store epistemic position in vertex mapping
    (puthash 'epistemic-position (rumsfeld-tetrahedron-geometric-position tetrahedron)
             (identity-wave-function-vertex-mapping wave-function))
    wave-function))

(defun wave-function-create-with-epistemic-state (id frequency epistemic-state)
  "Create wave function with integrated epistemic state"
  (let ((wave (wave-function-create-with-church-encoding id frequency)))
    (rumsfeld-tetrahedron-integrate-with-wave-function epistemic-state wave)
    wave))

;;; Epistemic State Visualization

(defun rumsfeld-tetrahedron-visualize (tetrahedron)
  "Visualize epistemic state as ASCII tetrahedron"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (let ((known-knowns (hash-table-count (rumsfeld-tetrahedron-known-knowns tetrahedron)))
          (known-unknowns (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron)))
          (unknown-knowns (hash-table-count (rumsfeld-tetrahedron-unknown-knowns tetrahedron)))
          (unknown-unknowns (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))))
      (format "
Epistemic Tetrahedron:
        Unknown Unknowns (UU): %d
               /|\\
              / | \\
         Known    Unknown
        Unknowns  Knowns
         (KU): %d  (UK): %d
               \\|/
        Known Knowns (KK): %d

Confidence: %.2f
Consciousness: %.2f
Evolution: %.2f"
              unknown-unknowns
              known-unknowns
              unknown-knowns
              known-knowns
              (rumsfeld-tetrahedron-epistemic-confidence tetrahedron)
              (rumsfeld-tetrahedron-consciousness-level tetrahedron)
              (rumsfeld-tetrahedron-evolution-capability tetrahedron)))))

;;; Debug and Inspection

(defun rumsfeld-tetrahedron-inspect (tetrahedron)
  "Inspect Rumsfeld tetrahedron"
  (when (rumsfeld-tetrahedron-p tetrahedron)
    (message "Rumsfeld Tetrahedron:
  Known Knowns: %d
  Known Unknowns: %d
  Unknown Knowns: %d
  Unknown Unknowns: %d
  Epistemic Confidence: %.2f
  Consciousness Level: %.2f
  Evolution Capability: %.2f
  Geometric Position: %S"
             (hash-table-count (rumsfeld-tetrahedron-known-knowns tetrahedron))
             (hash-table-count (rumsfeld-tetrahedron-known-unknowns tetrahedron))
             (hash-table-count (rumsfeld-tetrahedron-unknown-knowns tetrahedron))
             (hash-table-count (rumsfeld-tetrahedron-unknown-unknowns tetrahedron))
             (rumsfeld-tetrahedron-epistemic-confidence tetrahedron)
             (rumsfeld-tetrahedron-consciousness-level tetrahedron)
             (rumsfeld-tetrahedron-evolution-capability tetrahedron)
             (rumsfeld-tetrahedron-geometric-position tetrahedron))))

;;; Global Registry

(defvar rumsfeld-tetrahedron-registry (make-hash-table :test 'equal)
  "Registry of Rumsfeld tetrahedra by ID")

(defun rumsfeld-tetrahedron-register (tetrahedron id)
  "Register Rumsfeld tetrahedron in global registry"
  (puthash id tetrahedron rumsfeld-tetrahedron-registry)
  tetrahedron)

(defun rumsfeld-tetrahedron-get (id)
  "Get Rumsfeld tetrahedron by ID from registry"
  (gethash id rumsfeld-tetrahedron-registry))

;;; Initialize default epistemic state
(let ((default-epistemic-state (rumsfeld-tetrahedron-create "default")))
  (rumsfeld-tetrahedron-register default-epistemic-state "default"))

(provide 'wave-epistemic)

;;; wave-epistemic.el ends here

