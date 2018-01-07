(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'zmq)
(require 'hmac-def)

(defconst jupyter-protocol-version "5.3"
  "The jupyter protocol version that is implemented.")

(defconst jupyter-socket-types
  (list :hb zmq-REQ
        :shell zmq-DEALER
        :iopub zmq-SUB
        :stdin zmq-DEALER
        :control zmq-DEALER)
  "The socket types for the various channels used by `jupyter'.")

(defconst jupyter-message-types
  (list :execute-result "execute_result"
        :execute-request "execute_request"
        :execute-reply "execute_reply"
        :inspect-request "inspect_request"
        :inspect-reply "inspect_reply"
        :complete-request "complete_request"
        :complete-reply "complete_reply"
        :history-request "history_request"
        :history-reply "history_reply"
        :is-complete-request "is_complete_request"
        :is-complete-reply "is_complete_reply"
        :comm-info-request "comm_info_request"
        :comm-info-reply "comm_info_reply"
        :kernel-info-request "kernel_info_request"
        :kernel-info-reply "kernel_info_reply"
        :shutdown-request "shutdown_request"
        :shutdown-reply "shutdown_reply"
        :interupt-request "interrupt_request"
        :interrupt-reply "interrupt_reply"
        :stream "stream"
        :display-data "display_data"
        :update-display-data "update_display_data"
        :execute-input "execute_input"
        :error "error"
        :status "status"
        :clear-output "clear_output"
        :input-reply "input_reply")
  "A plist mapping keywords to Jupyter message type strings.
The plist values are the message types either sent or received
from the kernel.")

;; https://tools.ietf.org/html/rfc4868
(defun sha256 (object)
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function hmac-sha256 sha256 64 32)

;; TODO: Better UUID randomness, `cl-random' seeds the random state with the
;; current time but only to second resolution.
(defun jupyter-new-uuid ()
  "Make a version 4 UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (cl-random 65536)
          (cl-random 65536)
          (cl-random 65536)
          ;; https://tools.ietf.org/html/rfc4122
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                ;; ?l = little-endian
                (logior (logand r 4095) 16384)
              ;; big-endian
              (logior (logand r 65295) 64)))
          (let ((r (cl-random 65536)))
            (if (= (byteorder) ?l)
                (logior (logand r 49151) 32768)
              (logior (logand r 65471) 128)))
          (cl-random 16777216)
          (cl-random 16777216)))

;;; Session object definition

(cl-defstruct (jupyter-session
               (:constructor nil)
               (:constructor
                jupyter-session
                (&key (key nil) &aux (id (jupyter-new-uuid)))))
  (id nil :read-only t)
  (key nil :read-only t))

;;; Request object definition

;; A `jupyter-request' object represents the status of a request to the kernel
;; and holds all the information required to process the messages associated
;; with the request. Whenever a message arrives that is associated with a
;; request's `jupyter-request-id', any callbacks associated with the message
;; type are run (see `jupyter-add-callback'). When a request's
;; `jupyter-idle-received-p' property is non-nil, then it signifies that the
;; request has been handled by the kernel.
(cl-defstruct jupyter-request
  ;; NOTE Use `jupyter-request-id' instead of `jupyter-request--id'
  (-id)
  (time (current-time))
  (idle-received-p nil)
  (run-handlers-p t)
  (callbacks))

(defun jupyter-request-id (req)
  "Get the message ID for REQ."
  (with-timeout (0.5 (error "Request not processed"))
    (while (null (jupyter-request--id req))
      (sleep-for 0 10)))
  (jupyter-request--id req))

(defun jupyter-request-inhibit-handlers (req)
  "Inhibit the execution of a `jupyter-kernel-client's handlers for REQ.
Sets `jupyter-request-run-handlers-p' to nil for REQ and returns
REQ. This function is intended to be a convenience function so
that you can do:

    (jupyter-add-callback 'execute-reply
        (jupyter-request-inhibit-handlers
          (jupyter-execute-request client ...))
      (lambda (msg) ...))"
  (setf (jupyter-request-run-handlers-p req) nil)
  req)

(provide 'jupyter-base)
