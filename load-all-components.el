;;; load-all-components.el --- Load all wave function components in correct order

;; This file loads all components in the correct dependency order
;; for comprehensive testing and validation

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load components in dependency order
(message "Loading Wave Function Core...")
(load-file "wave-function-core.el")

(message "Loading Wave Function Engine...")
(load-file "wave-function-engine.el")

(message "Loading Geometric Solids...")
(load-file "wave-geometric-solids.el")

(message "Loading Archimedean Solids...")
(load-file "wave-archimedean.el")

(message "Loading Rosette Manifolds...")
(load-file "wave-rosette-manifolds.el")

(message "Loading Multiplexer System...")
(load-file "wave-multiplexer.el")

(message "Loading Communication Protocols...")
(load-file "wave-communication.el")

(message "Loading Epistemic System...")
(load-file "wave-epistemic.el")

(message "Loading Identity Management...")
(load-file "wave-identity-management.el")

(message "Loading Autonomous System...")
(load-file "wave-autonomous.el")

(message "Loading Emacs Integration...")
(load-file "wave-emacs-integration.el")

(message "All components loaded successfully!")

(provide 'load-all-components)
