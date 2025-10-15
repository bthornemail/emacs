;;; wave-protocol-adapter.el --- Multi-protocol communication layer

;; Copyright (C) 2024 Axiomatic
;; Author: H²GNN Development Assistant
;; Version: 1.0.0
;; Keywords: protocol, emacsclient, websocket, mqtt, communication, wave-function
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (wave-y-combinator "1.0") (wave-async-framework "1.0"))

;;; Commentary:
;; Implements multi-protocol communication layer with incidence relation tracking:
;; - Emacs Client: External connections to Emacs server
;; - WebSocket: Browser/external connections via HTTP
;; - MQTT: IoT/message-based communication
;; - Incidence Relations: Track geometric communication patterns
;; - Message Queuing: Asynchronous message handling
;; - Protocol Switching: Dynamic protocol selection

;;; Code:

(require 'cl-lib)
(require 'wave-y-combinator)
(require 'wave-async-framework)

;;; Protocol Adapter Data Structures

(cl-defstruct wave-protocol-adapter
  "Protocol adapter for multi-protocol communication"
  (adapter-id "" :type string)
  (protocol-type 'emacsclient :type symbol)  ; emacsclient, websocket, mqtt
  (connection nil)
  (connection-state 'disconnected :type symbol)  ; disconnected, connecting, connected, error
  (message-queue nil :type list)
  (incidence-relations (make-hash-table :test 'equal) :type hash-table)
  (message-handlers (make-hash-table :test 'equal) :type hash-table)
  (error-handlers (make-hash-table :test 'equal) :type hash-table)
  (config nil :type (or plist null))
  (last-message-time nil :type (or float null))
  (message-count 0 :type integer)
  (error-count 0 :type integer))

(cl-defstruct wave-protocol-message
  "Message structure for protocol communication"
  (message-id "" :type string)
  (from "" :type string)
  (to "" :type string)
  (content "" :type string)
  (message-type 'text :type symbol)  ; text, command, data, control
  (timestamp nil :type (or float null))
  (incidence-data nil :type (or plist null))
  (geometric-metadata nil :type (or plist null))
  (spo-context nil :type (or plist null))
  (priority 0 :type integer)
  (retry-count 0 :type integer))

(cl-defstruct wave-protocol-connection
  "Connection information for protocol adapters"
  (connection-id "" :type string)
  (protocol-type 'emacsclient :type symbol)
  (host "" :type string)
  (port 0 :type integer)
  (credentials nil :type (or plist null))
  (timeout 30.0 :type float)
  (retry-attempts 3 :type integer)
  (connection-object nil)
  (state 'disconnected :type symbol)
  (last-activity nil :type (or float null))
  (message-history nil :type list))

;;; Emacs Client Protocol

(defun wave-protocol-emacsclient-init (&optional server-name)
  "Initialize Emacs client server for external connections"
  (require 'server)
  (let ((server-name (or server-name "wave-function-server")))
    (unless (server-running-p server-name)
      (server-start server-name))
    (message "Emacs client server initialized: %s" server-name)
    t))

(defun wave-protocol-emacsclient-create-adapter (adapter-id &optional config)
  "Create Emacs client protocol adapter"
  (let ((adapter (make-wave-protocol-adapter
                  :adapter-id adapter-id
                  :protocol-type 'emacsclient
                  :config (or config (list :server-name "wave-function-server")))))
    (wave-protocol-adapter-register adapter)
    adapter))

(defun wave-protocol-emacsclient-connect (adapter)
  "Connect Emacs client adapter"
  (when (eq (wave-protocol-adapter-protocol-type adapter) 'emacsclient)
    (setf (wave-protocol-adapter-connection-state adapter) 'connecting)
    (let ((server-name (plist-get (wave-protocol-adapter-config adapter) :server-name)))
      (if (server-running-p server-name)
          (progn
            (setf (wave-protocol-adapter-connection-state adapter) 'connected)
            (setf (wave-protocol-adapter-connection adapter) server-name)
            (message "Emacs client adapter %s connected to server %s" 
                     (wave-protocol-adapter-adapter-id adapter) server-name)
            t)
        (progn
          (setf (wave-protocol-adapter-connection-state adapter) 'error)
          (message "Emacs client adapter %s failed to connect: server not running" 
                   (wave-protocol-adapter-adapter-id adapter))
          nil)))))

(defun wave-protocol-emacsclient-send (adapter message)
  "Send message via Emacs client protocol"
  (when (and (eq (wave-protocol-adapter-connection-state adapter) 'connected)
             (wave-protocol-message-p message))
    (let ((server-name (wave-protocol-adapter-connection adapter)))
      (condition-case err
          (progn
            (server-eval-at server-name 
                           `(wave-protocol-receive-message ,(wave-protocol-message-content message)))
            (setf (wave-protocol-adapter-last-message-time adapter) (float-time))
            (setf (wave-protocol-adapter-message-count adapter) 
                  (1+ (wave-protocol-adapter-message-count adapter)))
            t)
        (error
         (setf (wave-protocol-adapter-error-count adapter) 
               (1+ (wave-protocol-adapter-error-count adapter)))
         (message "Emacs client send error: %s" (error-message-string err))
         nil)))))

;;; WebSocket Protocol

(defun wave-protocol-websocket-init (port &optional host)
  "Initialize WebSocket server for browser/external connections"
  (let ((host (or host "localhost"))
        (port (or port 8080)))
    (condition-case err
        (progn
          (require 'websocket)
          (let ((server (websocket-server
                         port
                         :host host
                         :on-message 'wave-protocol-websocket-on-message
                         :on-open 'wave-protocol-websocket-on-open
                         :on-close 'wave-protocol-websocket-on-close)))
            (message "WebSocket server initialized on %s:%d" host port)
            server))
      (error
       (message "WebSocket server initialization failed: %s" (error-message-string err))
       nil))))

(defun wave-protocol-websocket-create-adapter (adapter-id &optional config)
  "Create WebSocket protocol adapter"
  (let ((adapter (make-wave-protocol-adapter
                  :adapter-id adapter-id
                  :protocol-type 'websocket
                  :config (or config (list :host "localhost" :port 8080)))))
    (wave-protocol-adapter-register adapter)
    adapter))

(defun wave-protocol-websocket-connect (adapter)
  "Connect WebSocket adapter"
  (when (eq (wave-protocol-adapter-protocol-type adapter) 'websocket)
    (setf (wave-protocol-adapter-connection-state adapter) 'connecting)
    (let ((host (plist-get (wave-protocol-adapter-config adapter) :host))
          (port (plist-get (wave-protocol-adapter-config adapter) :port)))
      (condition-case err
          (progn
            (let ((websocket (websocket-open (format "ws://%s:%d" host port)
                                            :on-message 'wave-protocol-websocket-on-message
                                            :on-open 'wave-protocol-websocket-on-open
                                            :on-close 'wave-protocol-websocket-on-close)))
              (setf (wave-protocol-adapter-connection adapter) websocket)
              (setf (wave-protocol-adapter-connection-state adapter) 'connected)
              (message "WebSocket adapter %s connected to %s:%d" 
                       (wave-protocol-adapter-adapter-id adapter) host port)
              t))
        (error
         (setf (wave-protocol-adapter-connection-state adapter) 'error)
         (message "WebSocket adapter %s connection failed: %s" 
                  (wave-protocol-adapter-adapter-id adapter) (error-message-string err))
         nil)))))

(defun wave-protocol-websocket-send (adapter message)
  "Send message via WebSocket protocol"
  (when (and (eq (wave-protocol-adapter-connection-state adapter) 'connected)
             (wave-protocol-message-p message))
    (let ((websocket (wave-protocol-adapter-connection adapter)))
      (condition-case err
          (progn
            (websocket-send-text websocket (wave-protocol-message-content message))
            (setf (wave-protocol-adapter-last-message-time adapter) (float-time))
            (setf (wave-protocol-adapter-message-count adapter) 
                  (1+ (wave-protocol-adapter-message-count adapter)))
            t)
        (error
         (setf (wave-protocol-adapter-error-count adapter) 
               (1+ (wave-protocol-adapter-error-count adapter)))
         (message "WebSocket send error: %s" (error-message-string err))
         nil)))))

(defun wave-protocol-websocket-on-message (websocket frame)
  "Handle incoming WebSocket message"
  (let ((message-content (websocket-frame-payload frame)))
    (wave-protocol-handle-incoming-message 'websocket message-content)))

(defun wave-protocol-websocket-on-open (websocket)
  "Handle WebSocket connection open"
  (message "WebSocket connection opened"))

(defun wave-protocol-websocket-on-close (websocket)
  "Handle WebSocket connection close"
  (message "WebSocket connection closed"))

;;; MQTT Protocol

(defun wave-protocol-mqtt-init (broker-url &optional topics)
  "Initialize MQTT client for IoT/message-based communication"
  (let ((topics (or topics '("wave-function/commands" "wave-function/data" "wave-function/control"))))
    (condition-case err
        (progn
          (require 'mqtt)
          (let ((mqtt-client (mqtt-connect broker-url)))
            (dolist (topic topics)
              (mqtt-subscribe mqtt-client topic 'wave-protocol-mqtt-on-message))
            (message "MQTT client initialized: %s" broker-url)
            mqtt-client))
      (error
       (message "MQTT client initialization failed: %s" (error-message-string err))
       nil))))

(defun wave-protocol-mqtt-create-adapter (adapter-id &optional config)
  "Create MQTT protocol adapter"
  (let ((adapter (make-wave-protocol-adapter
                  :adapter-id adapter-id
                  :protocol-type 'mqtt
                  :config (or config (list :broker-url "tcp://localhost:1883" 
                                          :topics '("wave-function/commands"))))))
    (wave-protocol-adapter-register adapter)
    adapter))

(defun wave-protocol-mqtt-connect (adapter)
  "Connect MQTT adapter"
  (when (eq (wave-protocol-adapter-protocol-type adapter) 'mqtt)
    (setf (wave-protocol-adapter-connection-state adapter) 'connecting)
    (let ((broker-url (plist-get (wave-protocol-adapter-config adapter) :broker-url))
          (topics (plist-get (wave-protocol-adapter-config adapter) :topics)))
      (condition-case err
          (progn
            (let ((mqtt-client (mqtt-connect broker-url)))
              (dolist (topic topics)
                (mqtt-subscribe mqtt-client topic 'wave-protocol-mqtt-on-message))
              (setf (wave-protocol-adapter-connection adapter) mqtt-client)
              (setf (wave-protocol-adapter-connection-state adapter) 'connected)
              (message "MQTT adapter %s connected to %s" 
                       (wave-protocol-adapter-adapter-id adapter) broker-url)
              t))
        (error
         (setf (wave-protocol-adapter-connection-state adapter) 'error)
         (message "MQTT adapter %s connection failed: %s" 
                  (wave-protocol-adapter-adapter-id adapter) (error-message-string err))
         nil)))))

(defun wave-protocol-mqtt-send (adapter message)
  "Send message via MQTT protocol"
  (when (and (eq (wave-protocol-adapter-connection-state adapter) 'connected)
             (wave-protocol-message-p message))
    (let ((mqtt-client (wave-protocol-adapter-connection adapter))
          (topic (plist-get (wave-protocol-adapter-config adapter) :default-topic "wave-function/messages")))
      (condition-case err
          (progn
            (mqtt-publish mqtt-client topic (wave-protocol-message-content message))
            (setf (wave-protocol-adapter-last-message-time adapter) (float-time))
            (setf (wave-protocol-adapter-message-count adapter) 
                  (1+ (wave-protocol-adapter-message-count adapter)))
            t)
        (error
         (setf (wave-protocol-adapter-error-count adapter) 
               (1+ (wave-protocol-adapter-error-count adapter)))
         (message "MQTT send error: %s" (error-message-string err))
         nil)))))

(defun wave-protocol-mqtt-on-message (topic message)
  "Handle incoming MQTT message"
  (wave-protocol-handle-incoming-message 'mqtt message topic))

;;; Message Management

(defun wave-protocol-message-create (content &optional from to message-type)
  "Create protocol message"
  (make-wave-protocol-message
   :message-id (format "msg-%d" (random 10000))
   :from (or from "wave-function")
   :to (or to "unknown")
   :content content
   :message-type (or message-type 'text)
   :timestamp (float-time)
   :priority 0
   :retry-count 0))

(defun wave-protocol-message-set-incidence-data (message incidence-data)
  "Set incidence relation data for message"
  (setf (wave-protocol-message-incidence-data message) incidence-data)
  message)

(defun wave-protocol-message-set-geometric-metadata (message geometric-metadata)
  "Set geometric metadata for message"
  (setf (wave-protocol-message-geometric-metadata message) geometric-metadata)
  message)

(defun wave-protocol-message-set-spo-context (message spo-context)
  "Set SPO context for message"
  (setf (wave-protocol-message-spo-context message) spo-context)
  message)

;;; Protocol Adapter Management

(defun wave-protocol-adapter-connect (adapter)
  "Connect protocol adapter"
  (pcase (wave-protocol-adapter-protocol-type adapter)
    ('emacsclient (wave-protocol-emacsclient-connect adapter))
    ('websocket (wave-protocol-websocket-connect adapter))
    ('mqtt (wave-protocol-mqtt-connect adapter))
    (_ (error "Unknown protocol type: %s" (wave-protocol-adapter-protocol-type adapter)))))

(defun wave-protocol-adapter-send (adapter message)
  "Send message through protocol adapter with incidence tracking"
  (when (wave-protocol-adapter-connect adapter)
    (let ((result (pcase (wave-protocol-adapter-protocol-type adapter)
                    ('emacsclient (wave-protocol-emacsclient-send adapter message))
                    ('websocket (wave-protocol-websocket-send adapter message))
                    ('mqtt (wave-protocol-mqtt-send adapter message))
                    (_ nil))))
      (when result
        (wave-protocol-track-incidence-relation adapter message))
      result)))

(defun wave-protocol-adapter-disconnect (adapter)
  "Disconnect protocol adapter"
  (setf (wave-protocol-adapter-connection-state adapter) 'disconnected)
  (setf (wave-protocol-adapter-connection adapter) nil)
  (message "Protocol adapter %s disconnected" (wave-protocol-adapter-adapter-id adapter)))

(defun wave-protocol-adapter-send-async (adapter message &optional callback error-callback)
  "Send message asynchronously through protocol adapter"
  (let ((operation (wave-async-operation-create
                    (format "protocol-send-%s" (wave-protocol-adapter-adapter-id adapter))
                    (lambda () (wave-protocol-adapter-send adapter message))
                    'timer)))
    (when callback
      (wave-async-operation-set-callbacks operation callback error-callback))
    (wave-async-execute operation)
    operation))

;;; Incidence Relation Tracking

(defun wave-protocol-track-incidence-relation (adapter message)
  "Track incidence relations for geometric communication"
  (let ((incidence-data (wave-protocol-message-incidence-data message))
        (geometric-metadata (wave-protocol-message-geometric-metadata message)))
    (when incidence-data
      (puthash (wave-protocol-message-message-id message) incidence-data
               (wave-protocol-adapter-incidence-relations adapter)))
    (when geometric-metadata
      (wave-protocol-update-geometric-state adapter geometric-metadata))))

(defun wave-protocol-update-geometric-state (adapter geometric-metadata)
  "Update geometric state based on message metadata"
  (let ((fano-plane-data (plist-get geometric-metadata :fano-plane))
        (5-cell-data (plist-get geometric-metadata :5-cell))
        (golden-ratio-data (plist-get geometric-metadata :golden-ratio)))
    (when fano-plane-data
      (wave-protocol-update-fano-plane-incidence adapter fano-plane-data))
    (when 5-cell-data
      (wave-protocol-update-5-cell-incidence adapter 5-cell-data))
    (when golden-ratio-data
      (wave-protocol-update-golden-ratio-incidence adapter golden-ratio-data))))

(defun wave-protocol-update-fano-plane-incidence (adapter fano-plane-data)
  "Update Fano plane incidence relations"
  (let ((points (plist-get fano-plane-data :points))
        (lines (plist-get fano-plane-data :lines)))
    (puthash 'fano-plane-incidence (list :points points :lines lines)
             (wave-protocol-adapter-incidence-relations adapter))))

(defun wave-protocol-update-5-cell-incidence (adapter 5-cell-data)
  "Update 5-cell incidence relations"
  (let ((vertices (plist-get 5-cell-data :vertices))
        (edges (plist-get 5-cell-data :edges))
        (faces (plist-get 5-cell-data :faces)))
    (puthash '5-cell-incidence (list :vertices vertices :edges edges :faces faces)
             (wave-protocol-adapter-incidence-relations adapter))))

(defun wave-protocol-update-golden-ratio-incidence (adapter golden-ratio-data)
  "Update golden ratio incidence relations"
  (let ((phi-value (plist-get golden-ratio-data :phi))
        (scaling-factor (plist-get golden-ratio-data :scaling-factor)))
    (puthash 'golden-ratio-incidence (list :phi phi-value :scaling scaling-factor)
             (wave-protocol-adapter-incidence-relations adapter))))

;;; Message Handling

(defun wave-protocol-handle-incoming-message (protocol-type content &optional topic)
  "Handle incoming message from any protocol"
  (let ((message (wave-protocol-message-create content "external" "wave-function" 'incoming)))
    (wave-protocol-message-set-incidence-data message 
                                             (list :protocol protocol-type :topic topic))
    (wave-protocol-process-incoming-message message)))

(defun wave-protocol-process-incoming-message (message)
  "Process incoming message and route to appropriate handlers"
  (let ((message-type (wave-protocol-message-message-type message))
        (content (wave-protocol-message-content message)))
    (pcase message-type
      ('command
       (wave-protocol-handle-command-message message))
      ('data
       (wave-protocol-handle-data-message message))
      ('control
       (wave-protocol-handle-control-message message))
      (_
       (wave-protocol-handle-text-message message)))))

(defun wave-protocol-handle-command-message (message)
  "Handle command message"
  (let ((content (wave-protocol-message-content message)))
    (condition-case err
        (eval (read content))
      (error
       (message "Command execution error: %s" (error-message-string err))))))

(defun wave-protocol-handle-data-message (message)
  "Handle data message"
  (let ((content (wave-protocol-message-content message))
        (incidence-data (wave-protocol-message-incidence-data message)))
    (message "Received data message: %s" content)
    (when incidence-data
      (wave-protocol-process-incidence-data incidence-data))))

(defun wave-protocol-handle-control-message (message)
  "Handle control message"
  (let ((content (wave-protocol-message-content message)))
    (message "Received control message: %s" content)))

(defun wave-protocol-handle-text-message (message)
  "Handle text message"
  (let ((content (wave-protocol-message-content message)))
    (message "Received text message: %s" content)))

(defun wave-protocol-process-incidence-data (incidence-data)
  "Process incidence relation data"
  (let ((protocol (plist-get incidence-data :protocol))
        (topic (plist-get incidence-data :topic)))
    (message "Processing incidence data: protocol=%s, topic=%s" protocol topic)))

;;; Message Queue Management

(defun wave-protocol-adapter-queue-message (adapter message)
  "Queue message for sending"
  (push message (wave-protocol-adapter-message-queue adapter))
  (wave-protocol-adapter-process-queue adapter))

(defun wave-protocol-adapter-process-queue (adapter)
  "Process message queue for adapter"
  (let ((queue (wave-protocol-adapter-message-queue adapter)))
    (while queue
      (let ((message (pop queue)))
        (when (wave-protocol-adapter-send adapter message)
          (setf (wave-protocol-adapter-message-queue adapter) queue)
          (setq queue nil))))))

;;; Registry and Management

(defvar wave-protocol-adapter-registry (make-hash-table :test 'equal)
  "Registry of protocol adapters by ID")

(defun wave-protocol-adapter-register (adapter)
  "Register protocol adapter in global registry"
  (puthash (wave-protocol-adapter-adapter-id adapter) adapter
           wave-protocol-adapter-registry)
  adapter)

(defun wave-protocol-adapter-get (adapter-id)
  "Get protocol adapter by ID from registry"
  (gethash adapter-id wave-protocol-adapter-registry))

(defun wave-protocol-adapter-list-all ()
  "List all registered protocol adapters"
  (let ((result nil))
    (maphash (lambda (id adapter) (push id result)) wave-protocol-adapter-registry)
    result))

;;; Inspection and Debug

(defun wave-protocol-adapter-inspect (adapter)
  "Inspect protocol adapter"
  (when (wave-protocol-adapter-p adapter)
    (message "Protocol Adapter: %s
  Type: %s
  State: %s
  Messages Sent: %d
  Errors: %d
  Last Message: %.2f
  Queue Length: %d
  Incidence Relations: %d"
             (wave-protocol-adapter-adapter-id adapter)
             (wave-protocol-adapter-protocol-type adapter)
             (wave-protocol-adapter-connection-state adapter)
             (wave-protocol-adapter-message-count adapter)
             (wave-protocol-adapter-error-count adapter)
             (wave-protocol-adapter-last-message-time adapter)
             (length (wave-protocol-adapter-message-queue adapter))
             (hash-table-count (wave-protocol-adapter-incidence-relations adapter)))))

(defun wave-protocol-message-inspect (message)
  "Inspect protocol message"
  (when (wave-protocol-message-p message)
    (message "Protocol Message: %s
  From: %s
  To: %s
  Type: %s
  Content: %s
  Timestamp: %.2f
  Priority: %d
  Retry Count: %d"
             (wave-protocol-message-message-id message)
             (wave-protocol-message-from message)
             (wave-protocol-message-to message)
             (wave-protocol-message-message-type message)
             (wave-protocol-message-content message)
             (wave-protocol-message-timestamp message)
             (wave-protocol-message-priority message)
             (wave-protocol-message-retry-count message))))

;;; Test Functions

(defun wave-protocol-test-emacsclient ()
  "Test Emacs client protocol"
  (interactive)
  (let ((adapter (wave-protocol-emacsclient-create-adapter "test-emacsclient")))
    (wave-protocol-adapter-inspect adapter)
    (when (wave-protocol-adapter-connect adapter)
      (let ((message (wave-protocol-message-create "test message" "test" "wave-function" 'text)))
        (wave-protocol-adapter-send adapter message)))
    (message "Emacs client protocol test completed!")))

(defun wave-protocol-test-websocket ()
  "Test WebSocket protocol"
  (interactive)
  (let ((adapter (wave-protocol-websocket-create-adapter "test-websocket")))
    (wave-protocol-adapter-inspect adapter)
    (message "WebSocket protocol test completed!")))

(defun wave-protocol-test-mqtt ()
  "Test MQTT protocol"
  (interactive)
  (let ((adapter (wave-protocol-mqtt-create-adapter "test-mqtt")))
    (wave-protocol-adapter-inspect adapter)
    (message "MQTT protocol test completed!")))

(defun wave-protocol-test-all ()
  "Test all protocol adapters"
  (interactive)
  (message "Testing protocol adapters...")
  (wave-protocol-test-emacsclient)
  (wave-protocol-test-websocket)
  (wave-protocol-test-mqtt)
  (message "Protocol adapter tests completed!"))

;;; Global Message Handler

(defun wave-protocol-receive-message (content)
  "Global message receiver for external protocols"
  (message "Received external message: %s" content)
  (wave-protocol-handle-incoming-message 'external content))

(provide 'wave-protocol-adapter)

;;; wave-protocol-adapter.el ends here
