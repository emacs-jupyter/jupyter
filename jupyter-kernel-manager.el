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
(require 'jupyter-connection)
(require 'jupyter-messages)
(require 'jupyter-client)

(declare-function string-trim-right "subr-x" (str))

(defgroup jupyter-kernel-manager nil
  "Jupyter kernel manager"
  :group 'communication)

(defclass jupyter-kernel-manager (jupyter-connection)
  ((name
    :initarg :name
    :type string)
   (conn-file
    :type (or null string))
   (kernel
    :type (or null process)
    :initform nil
    :documentation "The local kernel process or nil if no local
 kernel was started by this client.")
   (control-channel
    :type (or null jupyter-control-channel)
    :initform nil)
   (kernel-info
    :type (or null json-plist)
    :initform nil
    :documentation "Contains the result of the initial kernel_info_request
 to the kernel after starting the kernel.")
   (kernel-spec
    :type (or null json-plist)
    :initform nil)))

(cl-defmethod initialize-instance ((manager jupyter-kernel-manager) &rest slots)
  "Initialize MANAGER based on SLOTS.
If the `:name' slot is not found in SLOTS, it defaults to
\"python\". This means that without providing a kernel name, the
default kernel is a python kernel."
  (cl-call-next-method)
  (unless (slot-boundp manager 'name)
    (oset manager name "python")))

(cl-defmethod slot-unbound ((manager jupyter-kernel-manager) _class slot-name _fn)
  "Set default values for the SESSION and CONN-INFO slots of MANAGER.
When a MANAGER's `jupyter-connection' slots are missing set them
to their default values. For the `session' slot, set it to a new
`jupyter-session' with `:key' set to a new UUID. For the
`conn-info' slot, set it to the plist returned by a call to
`jupyter-create-connection-info' with `:kernel-name' being the
MANAGER's name slot and `:key' being the key of MANAGER's
session."
  (cond
   ((eq slot-name 'session)
    (oset manager session (jupyter-session :key (jupyter-new-uuid))))
   ((eq slot-name 'conn-info)
    (oset manager conn-info (jupyter-create-connection-info
                             :kernel-name (oref manager name)
                             :key (jupyter-session-key (oref manager session)))))
   (t (cl-call-next-method))))

(cl-defmethod jupyter-make-client ((manager jupyter-kernel-manager) class &rest slots)
  "Make a new client from CLASS connected to MANAGER's kernel.
CLASS should be a subclass of `jupyter-kernel-client', a new
instance of CLASS initialized with SLOTS is returned configured
to connect to MANAGER's kernel."
  (unless (child-of-class-p class 'jupyter-kernel-client)
    (signal 'wrong-type-argument (list '(subclass jupyter-kernel-client) class)))
  (let ((client (apply #'make-instance class slots)))
    (oset client parent-instance manager)
    (jupyter-initialize-connection client)
    client))

(defun jupyter--kernel-sentinel (manager kernel event)
  (cond
   ((cl-loop for type in '("exited" "failed" "finished" "killed" "deleted")
             thereis (string-prefix-p type event))
    (jupyter-stop-channels manager)
    ;; TODO: Only delete file when it hasn't been modified since it was created?
    (delete-file (oref manager conn-file))
    (oset manager kernel nil)
    (oset manager conn-file nil)
    (oset manager conn-info nil))))

(defun jupyter--start-kernel (kernel-name env args)
  "Start a kernel.
A kernel named KERNEL-NAME is started using ARGS. The name of the
command used to start the kernel subprocess should be the first
element of ARGS and the rest of the elements of ARGS are the
command line parameters passed to the command. If ENV is non-nil,
then it should be a plist containing environment variable names
as keywords along with their corresponding values. These will be
set as the process environment before starting the kernel.

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
           process-environment)))
    (apply #'start-process
           (format "jupyter-kernel-%s" kernel-name)
           nil (car args) (cdr args))))

;; TODO: Allow passing arguments like a different kernel file name or different
;; ports and arguments to the kernel
(cl-defmethod jupyter-start-kernel ((manager jupyter-kernel-manager))
  "Start a kernel and associate it with MANAGER.

The MANAGER's `name' property is passed to
`jupyter-find-kernelspec' in order to find the kernel to start,
this means that `name' can be a prefix of a kernel name as well
as a full kernel name. For example, if `name' is \"julia\" it
will match the full kernel names \"julia-0.6\", \"julia-0.4\",
etc. The kernel used will be the first one matched from the list
of kernels returned by:

    jupyter kernelspec list

If a valid kernel is found, its kernelspec is used to start a new
kernel. Starting a kernel involves the following steps:

1. Generating a new connection info with random ports for the
   channels. See `jupyter-create-connection-info'.

2. Assigning a new `jupyter-session' to the MANAGER using the
   generated key from the connection info. (TODO: Should first
   start with generating a session key and then assigning it to
   the connection info)

3. Writing the connection info to file

4. Starting a new subprocess kernel

5. Starting a control channel for the MANAGER to send
shutdown/interrupt requests"
  (let ((kname-spec (jupyter-find-kernelspec (oref manager name))))
    (unless kname-spec
      (error "No kernel found that starts with name (%s)" (oref manager name)))
    (cl-destructuring-bind (kernel-name . spec) kname-spec
      ;; Ensure we use the full name of the kernel
      ;; TODO: Require a valid kernel name when initializing the manager
      (oset manager name kernel-name)
      (oset manager kernel-spec spec)
      ;; SESSION and CONN-INFO slots are automatically set to default values if
      ;; missing, see `slot-unbound' of `jupyter-kernel-manager'.
      (let* ((key (jupyter-session-key (oref manager session)))
             (name (oref manager name))
             (resource-dir (string-trim-right
                            (shell-command-to-string
                             "jupyter --runtime-dir")))
             (conn-file (expand-file-name
                         (concat "kernel-" key ".json")
                         resource-dir)))
        ;; Write the connection file
        (with-temp-file conn-file
          (let ((json-encoding-pretty-print t))
            (insert (json-encode-plist (oref manager conn-info)))))
        ;; Start the process
        (let ((atime (nth 4 (file-attributes conn-file)))
              (proc (jupyter--start-kernel
                     kernel-name (plist-get spec :env)
                     (cl-loop
                      for arg in (plist-get spec :argv)
                      if (equal arg "{connection_file}") collect conn-file
                      else if (equal arg "{resource_dir}") collect resource-dir
                      else collect arg))))
          ;; Block until the kernel reads the connection file
          (with-timeout
              (10 (delete-file conn-file)
                  (delete-process proc)
                  (error "Kernel did not read connection file within timeout."))
            (while (equal atime (nth 4 (file-attributes conn-file)))
              (sleep-for 0 100)))
          (oset manager kernel proc)
          (oset manager conn-file (expand-file-name
                                   (format "kernel-%d.json" (process-id proc))
                                   (file-name-directory conn-file)))
          (rename-file conn-file (oref manager conn-file))
          (set-process-sentinel
           proc (apply-partially #'jupyter--kernel-sentinel manager))
          (jupyter-start-channels manager)
          manager)))))

(cl-defmethod jupyter-start-channels ((manager jupyter-kernel-manager))
  "Start a control channel on MANAGER."
  (let ((control-channel (oref manager control-channel)))
    (if control-channel
        (unless (jupyter-channel-alive-p control-channel)
          (jupyter-start-channel
           control-channel
           :identity (jupyter-session-id (oref manager session))))
      (let ((conn-info (oref manager conn-info)))
        (oset manager control-channel
              (jupyter-control-channel
               :endpoint (format "%s://%s:%d"
                                 (plist-get conn-info :transport)
                                 (plist-get conn-info :ip)
                                 (plist-get conn-info :control_port))))
        (jupyter-start-channels manager)))))

(cl-defmethod jupyter-stop-channels ((manager jupyter-kernel-manager))
  "Stop the control channel on MANAGER."
  (let ((control-channel (oref manager control-channel)))
    (when control-channel
      (jupyter-stop-channel control-channel)
      (oset manager control-channel nil))))

(cl-defmethod jupyter-send ((manager jupyter-kernel-manager) type message)
  (unless (member type '("shutdown_request" "interrupt_request"))
    (error "Only shutdown or interrupt requests on control channel (%s)."
           type))
  (let ((session (oref manager session))
        (sock (oref (oref manager control-channel) socket))
        (res nil))
    (jupyter-send session sock type message)
    ;; FIXME: Should everything sent on the control channel be synchronous?
    (with-timeout (2 nil)
      (while (condition-case nil
                 (progn
                   (setq res (jupyter-recv session sock zmq-NOBLOCK))
                   nil)
               (zmq-EAGAIN t))
        (sleep-for 0 100))
      res)))

(cl-defmethod jupyter-stop-kernel ((manager jupyter-kernel-manager))
  (when (jupyter-kernel-alive-p manager)
    (jupyter-shutdown-request manager)
    (with-timeout (5 (delete-process (oref manager kernel)))
      (while (jupyter-kernel-alive-p manager)
        (sleep-for 0 100)))))

(cl-defmethod jupyter-kernel-alive-p ((manager jupyter-kernel-manager))
  (process-live-p (oref manager kernel)))

(cl-defmethod jupyter-shutdown-request ((manager jupyter-kernel-manager))
  "Request a shutdown of MANAGER's kernel.
If RESTART is non-nil, request a restart instead of a complete shutdown."
  ;; FIXME: This shutdown request doesn't seem to work
  (let ((msg (jupyter-message-shutdown-request)))
    (jupyter-send manager "shutdown_request" msg)))

(cl-defmethod jupyter-interrupt-request ((manager jupyter-kernel-manager))
  (if (equal (plist-get (oref manager kernel-spec) :interrupt_mode) "message")
      (let ((msg (jupyter-message-interrupt-request)))
        (jupyter-send manager "interrupt_request" msg))
    (interrupt-process (oref manager kernel) t)))

(defun jupyter-start-new-kernel (kernel-name &optional client-class)
  "Start a managed Jupyter kernel.
KERNEL-NAME is the name of the kernel to start. It can also be
the prefix of a valid kernel name, in which case the first kernel
in `jupyter-available-kernelspecs' that has a kernel name with
KERNEL-NAME as prefix will be used. Optional argument
CLIENT-CLASS should be a subclass of `jupyer-kernel-client' used
to initialize a new client connected to the kernel. CLIENT-CLASS
defaults to `jupyter-kernel-client'.

Return a cons cell (KM . KC) where KM is the
`jupyter-kernel-manager' that manages the kernel subprocess. KC
is a `jupyter-kernel-client' already connected to the kernel of
KM and whose class is CLIENT-CLASS. Note that KC has all channels
listening and the heartbeat channel un-paused."
  (setq client-class (or client-class 'jupyter-kernel-client))
  (unless (child-of-class-p client-class 'jupyter-kernel-client)
    (signal 'wrong-type-argument
            (list '(subclass jupyter-kernel-client) client-class)))
  (let (km kc)
    (setq km (jupyter-kernel-manager :name kernel-name))
    (setq kc (jupyter-make-client km client-class))
    (jupyter-start-channels kc)
    (jupyter-hb-unpause (oref kc hb-channel))
    (jupyter-start-kernel km)
    (condition-case nil
        (progn
          ;; Wait for the startup message
          (jupyter-wait-until-received :status (jupyter-missing-request kc) 10)
          ;; Remove the callback since the `jupyter-missing-request' does not go
          ;; through the normal request code path in `jupyter-kernel-client'
          (setf (jupyter-request-callbacks (jupyter-missing-request kc)) nil)
          (let ((info (jupyter-wait-until-received :kernel-info-reply
                        (jupyter-request-inhibit-handlers
                         (jupyter-kernel-info-request kc))
                        30)))
            (if info (oset km kernel-info (jupyter-message-content info))
              (error "Kernel did not respond to kernel-info request"))))
      ((error quit)
       (jupyter-stop-channels kc)
       (jupyter-stop-kernel km 0)))
    (cons km kc)))

(provide 'jupyter-kernel-manager)

;;; jupyter-kernel-manager.el ends here
