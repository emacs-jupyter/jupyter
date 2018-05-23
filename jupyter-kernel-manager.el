;;; jupyter-kernel-manager.el --- Jupyter kernel manager -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.edu>
;; Created: 08 Jan 2018
;; Version: 0.0.1
;; Keywords:
;; X-URL: https://github.com/nathan/jupyter-kernel-manager

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'jupyter-base)
(require 'jupyter-messages)
(require 'jupyter-client)

(declare-function string-trim-right "subr-x" (str))

(defgroup jupyter-kernel-manager nil
  "Jupyter kernel manager"
  :group 'jupyter)

(defclass jupyter-kernel-manager ()
  ((name
    :initarg :name
    :type string
    :documentation "The name of the kernel that is being managed.")
   (session
    :type jupyter-session
    :initarg :session
    :documentation "The session object used to sign and
send/receive messages.")
   (conn-file
    :type (or null string)
    :documentation "The absolute path of the connection file when
the kernel is alive.")
   (kernel
    :type (or null process)
    :initform nil
    :documentation "The local kernel process when the kernel is
alive.")
   (control-channel
    :type (or null jupyter-sync-channel)
    :initform nil
    :documentation "A control channel to make shutdown and
interrupt requests to the kernel.")
   (spec
    :type (or null json-plist)
    :initarg :spec
    :initform nil
    :documentation "The kernelspec used to start/restart the kernel.")))

(cl-defmethod initialize-instance ((manager jupyter-kernel-manager) &rest _slots)
  "Initialize MANAGER based on SLOTS.
If the `:name' slot is not found in SLOTS, it defaults to
\"python\". This means that without providing a kernel name, the
default kernel is a python kernel."
  (cl-call-next-method)
  (unless (slot-boundp manager 'name)
    (oset manager name "python")))

(cl-defmethod destructor ((manager jupyter-kernel-manager) &rest _params)
  "Kill the kernel of MANAGER and stop its channels."
  ;; See `jupyter--kernel-sentinel' for other cleanup
  (when (processp (oref manager kernel))
    (delete-process (oref manager kernel)))
  (jupyter-stop-channels manager))

(cl-defgeneric jupyter-make-client ((manager jupyter-kernel-manager) class &rest slots)
  "Make a new client from CLASS connected to MANAGER's kernel.
SLOTS are the slots used to initialize the client with.")

(cl-defmethod jupyter-make-client ((manager jupyter-kernel-manager) class &rest slots)
  "Make a new client from CLASS connected to MANAGER's kernel.
CLASS should be a subclass of `jupyter-kernel-client', a new
instance of CLASS is initialized with SLOTS and configured to
connect to MANAGER's kernel."
  (unless (child-of-class-p class 'jupyter-kernel-client)
    (signal 'wrong-type-argument (list '(subclass jupyter-kernel-client) class)))
  (let ((client (apply #'make-instance class slots)))
    (prog1 client
      (jupyter-initialize-connection client (oref manager session))
      (oset client manager manager))))

(defun jupyter--kernel-sentinel (manager kernel _event)
  "Cleanup resources after kernel shutdown.
If MANAGER's KERNEL process terminates, i.e. when EVENT describes
an event in which the KERNEL process was killed: kill the process
buffer and delete MANAGER's connection file from the
`jupyter-runtime-directory'."
  (cond
   ((not (process-live-p kernel))
    (and (buffer-live-p (process-buffer kernel))
         (kill-buffer (process-buffer kernel)))
    (when (and (slot-boundp manager 'conn-file)
               (file-exists-p (oref manager conn-file)))
      (delete-file (oref manager conn-file)))
    (oset manager kernel nil)
    (oset manager conn-file nil))))

(defun jupyter--start-kernel (manager kernel-name env args)
  "Start a kernel.
For a `jupyter-kernel-manager', MANAGER, state a kernel named
KERNEL-NAME with ENV and ARGS.

If ENV is non-nil, then it should be a plist containing
environment variable names as keywords along with their
corresponding values. These will be set as the process
environment before starting the kernel.

ARGS should be a list of command line arguments used to start the
kernel process. The name of the command used to start the kernel
should be the first element of ARGS and the rest of the elements
of ARGS are the arguments of the command.

Return the newly created kernel process."
  (let* ((process-environment
          (append
           ;; The first entry takes precedence when duplicated variables
           ;; are found in `process-environment'
           (cl-loop
            for e on env by #'cddr
            for k = (car e)
            for v = (cadr e)
            collect (format "%s=%s" (cl-subseq (symbol-name k) 1) v))
           process-environment))
         (proc (apply #'start-process
                      (format "jupyter-kernel-%s" kernel-name)
                      (generate-new-buffer
                       (format " *jupyter-kernel[%s]*" kernel-name))
                      (car args) (cdr args))))
    (prog1 proc
      (set-process-sentinel
       proc (apply-partially #'jupyter--kernel-sentinel manager)))))

(cl-defgeneric jupyter-start-kernel ((manager jupyter-kernel-manager) &optional timeout)
  "Start a kernel based on MANAGER's slots. Wait until TIMEOUT for startup.")

;; TODO: Allow passing arguments like a different kernel file name or different
;; ports and arguments to the kernel
(cl-defmethod jupyter-start-kernel ((manager jupyter-kernel-manager) &optional timeout)
  "Start a kernel and associate it with MANAGER.

The MANAGER's `name' property is passed to
`jupyter-find-kernelspecs' in order to find the kernel to start.
If `jupyter-find-kernelspecs' returns multiple kernelspecs that
match `name', the first one on the list is used.

If a valid kernel is found, its kernelspec is used to start a new
kernel. Starting a kernel involves the following steps:

1. Write the connection info of MANAGER's session to a file in
   the `jupyter-runtime-directory'. Using the standard naming
   convention, i.e. after a call to `jupyter-start-kernel' the
   connection file will be named kernel-PID.json where PID is the
   process ID of the kernel subprocess.

2. Start a kernel subprocess passing the connection info file as
   the {connection_file} argument in the kernelspec argument
   vector of the kernel.

3. Connect the control channel of MANAGER to the kernel."
  (unless (jupyter-kernel-alive-p manager)
    (cl-destructuring-bind (kernel-name . (resource-dir . spec))
        (car (jupyter-find-kernelspecs (oref manager name)))
      (let* ((temporary-file-directory jupyter-runtime-directory)
             (session (oref manager session))
             (conn-info (jupyter-session-conn-info session))
             (conn-file (make-temp-file "emacs-kernel-" nil ".json"))
             (reporter (make-progress-reporter
                        (format "Starting %s kernel..." kernel-name))))
        ;; Write the connection info file
        (let ((json-encoding-pretty-print t))
          (with-temp-buffer
            (insert (json-encode-plist conn-info))
            (write-region (point-min) (point-max) conn-file)))
        ;; This is needed for reliability
        (sleep-for 0.5)
        ;; Start the process
        (let ((atime (nth 4 (file-attributes conn-file)))
              (proc (jupyter--start-kernel
                     manager kernel-name (plist-get spec :env)
                     (cl-loop
                      for arg in (append (plist-get spec :argv) nil)
                      if (equal arg "{connection_file}")
                      collect conn-file
                      else if (equal arg "{resource_dir}")
                      collect resource-dir
                      else collect arg))))
          ;; TODO: This is not reliable.
          ;;
          ;; Block until the kernel reads the connection file
          (with-timeout
              ((or timeout 5)
               (delete-process proc)
               (delete-file conn-file)
               (error "Kernel did not read connection file within timeout"))
            ;; TODO: This may fail on some systems see `file-attributes'
            (while (equal atime (nth 4 (file-attributes conn-file)))
              (progress-reporter-update reporter)
              (sleep-for 0 200)))
          (oset manager kernel proc)
          (oset manager conn-file conn-file)
          (jupyter-start-channels manager)
          (progress-reporter-done reporter)
          manager)))))

(cl-defmethod jupyter-start-channels ((manager jupyter-kernel-manager))
  "Start a control channel on MANAGER."
  (let ((session (oref manager session))
        (channel (oref manager control-channel)))
    (if channel
        (unless (jupyter-channel-alive-p channel)
          (jupyter-start-channel channel :identity (jupyter-session-id session)))
      (let ((conn-info (jupyter-session-conn-info session)))
        (oset manager control-channel
              (jupyter-sync-channel
               :type :control
               :session session
               :endpoint (format "%s://%s:%d"
                                 (plist-get conn-info :transport)
                                 (plist-get conn-info :ip)
                                 (plist-get conn-info :control_port))))
        (jupyter-start-channels manager)))))

(cl-defmethod jupyter-stop-channels ((manager jupyter-kernel-manager))
  "Stop the control channel on MANAGER."
  (let ((channel (oref manager control-channel)))
    (when channel
      (jupyter-stop-channel channel)
      (oset manager control-channel nil))))

(cl-defgeneric jupyter-shutdown-kernel ((manager jupyter-kernel-manager) &optional restart timeout)
  "Shutdown MANAGER's kernel with an optional RESTART.
Wait until TIMEOUT before forcibly shutting down the kernel.")

(cl-defmethod jupyter-shutdown-kernel ((manager jupyter-kernel-manager) &optional restart timeout)
  "Shutdown MANAGER's kernel with an optional RESTART.
If RESTART is non-nil, then restart the kernel after shutdown.
First send a shutdown request on the control channel to the
kernel. If the kernel has not shutdown within TIMEOUT, forcibly
kill the kernel subprocess. After shutdown the MANAGER's control
channel is stopped unless RESTART is non-nil."
  (when (jupyter-kernel-alive-p manager)
    (let ((session (oref manager session))
          (sock (oref (oref manager control-channel) socket))
          (msg (jupyter-message-shutdown-request :restart restart)))
      (jupyter-send session sock :shutdown-request msg)
      (with-timeout ((or timeout 1)
                     (delete-process (oref manager kernel))
                     (message "Kernel did not shutdown by request (%s)" (oref manager name)))
        (while (jupyter-kernel-alive-p manager)
          (sleep-for 0.01)))
      (if restart
          (jupyter-start-kernel manager)
        (jupyter-stop-channels manager)))))

(cl-defgeneric jupyter-interrupt-kernel ((manager jupyter-kernel-manager) &optional timeout)
  "Interrupt MANAGER's kernel.
When the kernel has an interrupt mode of \"message\" send an
interrupt request and wait until TIMEOUT for a reply.")

(cl-defmethod jupyter-interrupt-kernel ((manager jupyter-kernel-manager) &optional timeout)
  "Interrupt MANAGER's kernel.
If the kernel's interrupt mode is set to \"message\" send an
interrupt request on MANAGER's control channel and wait until
TIMEOUT for a reply. Otherwise if the kernel does not specify an
interrupt mode, send an interrupt signal to the kernel
subprocess."
  (pcase (plist-get (oref manager spec) :interrupt_mode)
    ("message"
     (let ((session (oref manager session))
           (sock (oref (oref manager control-channel) socket))
           (msg (jupyter-message-interrupt-request)))
       (jupyter-send session sock :interrupt-request msg)
       (with-timeout ((or timeout 1)
                      (message "No interrupt reply from kernel (%s)" (oref manager name)))
         (while (condition-case nil
                    (prog1 nil (jupyter-recv session sock zmq-NOBLOCK))
                  (zmq-EAGAIN t))
           (sleep-for 0.01)))))
    (_ (interrupt-process (oref manager kernel) t))))

(cl-defgeneric jupyter-kernel-alive-p ((manager jupyter-kernel-manager))
  "Return non-nil if MANAGER's kernel is alive, otherwise return nil.")

(cl-defmethod jupyter-kernel-alive-p ((manager jupyter-kernel-manager))
  "Is MANGER's kernel alive?"
  (when (oref manager kernel)
    (process-live-p (oref manager kernel))))

(defun jupyter-start-new-kernel (kernel-name &optional client-class)
  "Start a managed Jupyter kernel.
KERNEL-NAME is the name of the kernel to start. It can also be
the prefix of a valid kernel name, in which case the first kernel
in `jupyter-available-kernelspecs' that has KERNEL-NAME as a
prefix will be used. Optional argument CLIENT-CLASS is a subclass
of `jupyer-kernel-client' and will be used to initialize a new
client connected to the kernel. CLIENT-CLASS defaults to
`jupyter-kernel-client'.

Return a list (KM KC INFO) where KM is the
`jupyter-kernel-manager' that manages the lifetime of the kernel
subprocess. KC is a new client connected to the kernel whose
class is CLIENT-CLASS. The client is connected to the kernel with
all channels listening for messages and the heartbeat channel
unpaused. Note that the client's `manager' slot will also be set
to the kernel manager instance, see `jupyter-make-client'.
Finally, INFO is the kernel info plist obtained from a
`:kernel-info-request'."
  (or client-class (setq client-class 'jupyter-kernel-client))
  (unless (child-of-class-p client-class 'jupyter-kernel-client)
    (signal 'wrong-type-argument
            (list '(subclass jupyter-kernel-client) client-class)))

  (let ((match (car (jupyter-find-kernelspecs kernel-name))))
    (unless match
      (error "No kernel found that starts with name (%s)" kernel-name))
    (setq kernel-name (car match))

    (let* ((key (jupyter-new-uuid))
           (conn-info (jupyter-create-connection-info
                       :kernel-name kernel-name :key key))
           (session (jupyter-session :key key :conn-info conn-info))
           (manager (jupyter-kernel-manager
                     :name kernel-name
                     :spec (cddr match)
                     :session session))
           (client (jupyter-make-client manager client-class))
           kernel-info)
      (unwind-protect
          (let (reporter)
            (jupyter-start-channels client)
            (jupyter-hb-unpause client)
            ;; Ensure that the necessary hooks to catch the startup message are
            ;; in place before starting the kernel.
            ;;
            ;; NOTE: Startup messages have no parent header, hence the need for
            ;; `jupyter-include-other-output'.
            (let* ((jupyter-include-other-output t)
                   (started nil)
                   (cb (lambda (msg)
                         (setq started
                               (jupyter-message-status-starting-p msg)))))
              (jupyter-add-hook client 'jupyter-iopub-message-hook cb)
              (jupyter-start-kernel manager 10)
              (setq reporter (make-progress-reporter "Kernel starting up..."))
              (with-timeout (10 (error "Kernel did not send startup message"))
                (while (not started)
                  (progress-reporter-update reporter)
                  (sleep-for 0.02))
                (progress-reporter-done reporter))
              (jupyter-remove-hook client 'jupyter-iopub-message-hook cb))
            (setq reporter (make-progress-reporter "Requesting kernel info..."))
            (let ((jupyter-inhibit-handlers t))
              (setq kernel-info
                    (jupyter-message-content
                     (jupyter-wait-until-received :kernel-info-reply
                       (jupyter-send-kernel-info-request client)
                       ;; TODO: Make this timeout configurable? The
                       ;; python kernel starts up fast, but the Julia
                       ;; kernel not so much.
                       5)))
              (unless kernel-info
                (error "Kernel did not respond to kernel-info request"))
              (progress-reporter-done reporter))
            (list manager client kernel-info))
        (unless kernel-info
          (destructor client)
          (destructor manager))))))

(provide 'jupyter-kernel-manager)

;;; jupyter-kernel-manager.el ends here
