;;; jupyter-kernel-process-manager.el --- Manage kernel processes directly -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 08 Aug 2019

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
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

;; Jupyter Kernel manager for managing kernel processes directly in Emacs.

;;; Code:

(require 'jupyter-kernel-manager)
(require 'jupyter-env)
(require 'jupyter-kernelspec)
(eval-when-compile (require 'subr-x))

;;; `jupyter-kernel-process'

(defclass jupyter--kernel-process (jupyter--kernel)
  ((process
    :type process
    :documentation "The kernel process."))
  :documentation "A Jupyter kernel process.
Starts a kernel process using `start-file-process'.

If the kernel was started on a remote host, ensure that local
tunnels are created when setting the session slot after the
kernel starts.")

(cl-defmethod jupyter-kernel-alive-p ((kernel jupyter--kernel-process))
  (and (slot-boundp kernel 'process)
       (process-live-p (oref kernel process))))

(defun jupyter--start-kernel-process (name kernelspec conn-file)
  (let* ((process-name (format "jupyter-kernel-%s" name))
         (buffer-name (format " *jupyter-kernel[%s]*" name))
         (process-environment
          (append (jupyter-process-environment kernelspec)
                  process-environment))
         (args (jupyter-kernel-argv kernelspec conn-file))
         (atime (nth 4 (file-attributes conn-file)))
         (process (apply #'start-file-process process-name
                         (generate-new-buffer buffer-name)
                         (car args) (cdr args))))
    (set-process-query-on-exit-flag process jupyter--debug)
    ;; Wait until the connection file has been read before returning.
    ;; This is to give the kernel a chance to setup before sending it
    ;; messages.
    ;;
    ;; TODO: Replace with a check of the heartbeat channel.
    (jupyter-with-timeout
        ((format "Starting %s kernel process..." name)
         jupyter-long-timeout
         (unless (process-live-p process)
           (error "Kernel process exited:\n%s"
                  (with-current-buffer (process-buffer process)
                    (ansi-color-apply (buffer-string))))))
      ;; Windows systems may not have good time resolution when retrieving
      ;; the last access time of a file so we don't bother with checking that
      ;; the kernel has read the connection file and leave it to the
      ;; downstream initialization to ensure that we can communicate with a
      ;; kernel.
      (or (memq system-type '(ms-dos windows-nt cygwin))
          (let ((attribs (file-attributes conn-file)))
            ;; `file-attributes' can potentially return nil, in this case
            ;; just assume it has read the connection file so that we can
            ;; know for sure it is not connected if it fails to respond to
            ;; any messages we send it.
            (or (null attribs)
                (not (equal atime (nth 4 attribs)))))))
    process))

(defun jupyter--kernel-died-process-sentinel (kernel)
  "Return a sentinel function calling KERNEL's `jupyter-kernel-died' method.
The method will be called when the process exits or receives a
fatal signal."
  (cl-check-type kernel jupyter-kernel-lifetime)
  (let ((ref (jupyter-weak-ref kernel)))
    (lambda (process _)
      (when-let (kernel (and (memq (process-status process) '(exit signal))
                             (jupyter-weak-ref-resolve ref)))
        (jupyter-kernel-died kernel)))))

(cl-defmethod jupyter-start-kernel ((kernel jupyter--kernel-process) &rest _args)
  "Start a KERNEL process with ARGS."
  (oset kernel session (jupyter-session-with-random-ports))
  ;; This is here for stability when running the tests.  Sometimes the
  ;; kernel ports don't respond due to the hack done in
  ;; `jupyter-session-with-random-ports'.
  (sit-for 0.5)
  (let ((proc (jupyter--start-kernel-process
               (jupyter-kernel-name kernel)
               (oref kernel spec)
               (jupyter-write-connection-file (oref kernel session) kernel))))
    (oset kernel process proc)
    (setf (process-sentinel proc)
          (jupyter--kernel-died-process-sentinel kernel))))

(cl-defmethod jupyter-kill-kernel ((kernel jupyter--kernel-process))
  (with-slots (process) kernel
    (delete-process process)
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process))))
  (cl-call-next-method))

;;; `jupyter-kernel-process-manager'

(defclass jupyter-kernel-process-manager (jupyter-kernel-manager)
  ((control-channel
    :type (or null jupyter-zmq-channel)
    :initform nil
    :documentation "The kernel's control channel."))
  :documentation "Manages kernel processes directly in Emacs.")

;; FIXME: Do not hard-code the communication layer
(cl-defmethod jupyter-make-client ((manager jupyter-kernel-process-manager) _class &rest _slots)
  "Make a new client from CLASS connected to MANAGER's kernel.
CLASS should be a subclass of `jupyter-kernel-client', a new
instance of CLASS is initialized with SLOTS and configured to
connect to MANAGER's kernel."
  (let ((client (cl-call-next-method)))
    (with-slots (kernel) manager
      (prog1 client
        (require 'jupyter-channel-ioloop-comm)
        ;; TODO: We can also have the manager hold the kcomm object and just
        ;; pass a single kcomm object to all clients using this manager since the
        ;; kcomm broadcasts event to all connected clients.  This is more
        ;; efficient as it only uses one subprocess for every client connected to
        ;; a kernel.
        (oset client kcomm (make-instance
                            'jupyter-channel-ioloop-comm))
        (jupyter-comm-initialize client (oref kernel session))))))

(cl-defmethod jupyter-start-kernel :after ((manager jupyter-kernel-process-manager) &rest _args)
  "Some final setup after starting MANAGER's kernel.
Update the process sentinel of the kernel process to call
`jupyter-kernel-died' on the managed kernel when the process
exits.

Also start manager's control channel."
  (with-slots (kernel control-channel) manager
    (add-function
     :after (process-sentinel (oref kernel process))
     (jupyter--kernel-died-process-sentinel manager))
    (unless control-channel
      (cl-destructuring-bind (&key transport ip control_port &allow-other-keys)
          (jupyter-session-conn-info (oref kernel session))
        (require 'jupyter-zmq-channel)
        (oset manager control-channel
              (make-instance
               'jupyter-zmq-channel
               :type :control
               :session (oref kernel session)
               :endpoint (format "%s://%s:%d" transport ip control_port)))))
    (jupyter-start-channel control-channel)))

(cl-defmethod jupyter-shutdown-kernel ((manager jupyter-kernel-process-manager) &optional restart timeout)
  "Shutdown MANAGER's kernel with an optional RESTART.
If RESTART is non-nil, then restart the kernel after shutdown.
First send a shutdown request on the control channel to the
kernel.  If the kernel has not shutdown within TIMEOUT, forcibly
kill the kernel subprocess.  After shutdown the MANAGER's control
channel is stopped unless RESTART is non-nil."
  (when (jupyter-kernel-alive-p manager)
    (with-slots (control-channel kernel) manager
      (jupyter-send control-channel :shutdown-request
                    (jupyter-message-shutdown-request :restart restart))
      ;; FIXME: This doesn't work properly, the kernel sends a shutdown reply
      ;; but the process status cannot be determined correctly as it is still
      ;; considered alive.  This is mainly when using the
      ;; `jupyter-command-kernel' and probably has to do with the fact that the
      ;; kernel is launched by a python process instead of being launched
      ;; directly as a process by Emacs.
      (jupyter-with-timeout
          ((format "%s kernel shutting down..."
                   (jupyter-kernel-name kernel))
           (or timeout jupyter-default-timeout)
           (message "%s kernel did not shutdown by request"
                    (jupyter-kernel-name kernel))
           (jupyter-kill-kernel kernel))
        (not (jupyter-kernel-alive-p manager)))
      (if restart
          (jupyter-start-kernel manager)
        (when-let (channel (oref manager control-channel))
          (jupyter-stop-channel channel)
          (oset manager control-channel nil))))))

(cl-defmethod jupyter-interrupt-kernel ((manager jupyter-kernel-process-manager) &optional timeout)
  "Interrupt MANAGER's kernel.
If the kernel's interrupt mode is set to \"message\" send an
interrupt request on MANAGER's control channel and wait until
TIMEOUT for a reply.  Otherwise if the kernel does not specify an
interrupt mode, send an interrupt signal to the kernel
subprocess."
  (when (jupyter-kernel-alive-p manager)
    (with-slots (kernel) manager
      (cl-destructuring-bind (_name _resource-dir . spec) (oref kernel spec)
        (pcase (plist-get spec :interrupt_mode)
          ("message"
           (with-slots (control-channel) manager
             (jupyter-send control-channel :interrupt-request
                           (jupyter-message-interrupt-request))
             (jupyter-with-timeout
                 ((format "Interrupting %s kernel"
                          (jupyter-kernel-name kernel))
                  (or timeout jupyter-default-timeout)
                  (message "No interrupt reply from kernel (%s)"
                           (jupyter-kernel-name kernel)))
               (jupyter-recv control-channel 'dont-wait))))
          (_
           (if (object-of-class-p kernel 'jupyter--kernel-process)
               (interrupt-process (oref kernel process) t)
             (warn "Can't interrupt kernel"))))))))

(defun jupyter--error-if-no-kernel-info (client)
  (jupyter-kernel-info client))

(cl-defun jupyter-local-tcp-conn-info (&key
                                       (kernel-name "python")
                                       (signature-scheme "hmac-sha256")
                                       (key (jupyter-new-uuid))
                                       (hb-port 0)
                                       (stdin-port 0)
                                       (control-port 0)
                                       (shell-port 0)
                                       (iopub-port 0))
  "Return a connection info plist used to connect to a kernel.

The :transport key is set to \"tcp\" and the :ip key will be
\"127.0.0.1\".

The plist has the standard keys found in the jupyter spec.  See
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files.
A port number of 0 for a channel means to use a randomly assigned
port for that channel."
  (unless (or (= (length key) 0)
              (equal signature-scheme "hmac-sha256"))
    (error "Only hmac-sha256 signing is currently supported"))
  (append
   (list :kernel_name kernel-name
         :transport "tcp"
         :ip "127.0.0.1")
   (when (> (length key) 0)
     (list :signature_scheme signature-scheme
           :key key))
   (let ((ports (jupyter-available-local-ports
                 (cl-loop
                  with nports = 0
                  for p in (list hb-port stdin-port
                                 control-port shell-port
                                 iopub-port)
                  when (zerop p) do (cl-incf nports)
                  finally return nports))))
     (cl-loop
      for (channel . port) in `((:hb_port . ,hb-port)
                                (:stdin_port . ,stdin-port)
                                (:control_port . ,control-port)
                                (:shell_port . ,shell-port)
                                (:iopub_port . ,iopub-port))
      collect channel and if (= port 0)
      collect (pop ports) else collect port))))

(defun jupyter-start-new-kernel (kernel-name &optional client-class)
  "Start a managed Jupyter kernel.
KERNEL-NAME is the name of the kernel to start.  It can also be
the prefix of a valid kernel name, in which case the first kernel
in `jupyter-available-kernelspecs' that has KERNEL-NAME as a
prefix will be used.  Optional argument CLIENT-CLASS is a subclass
of `jupyer-kernel-client' and will be used to initialize a new
client connected to the kernel.  CLIENT-CLASS defaults to the
symbol `jupyter-kernel-client'.

Return a list (KM KC) where KM is the kernel manager managing the
lifetime of the kernel subprocess.  KC is a new client connected
to the kernel whose class is CLIENT-CLASS.  The client is
connected to the kernel with all channels listening for messages
and the heartbeat channel unpaused.  Note that the client's
`manager' slot will also be set to the kernel manager instance,
see `jupyter-make-client'.

Note, if `default-directory' is a remote directory, a kernel will
start on the remote host by using the \"jupyter kernel\" shell
command on the host."
  (or client-class (setq client-class 'jupyter-kernel-client))
  ;; TODO: Replace with
  ;; (cl-assert (child-of-class-p client-class 'jupyter-kernel-client))
  (jupyter-error-if-not-client-class-p client-class)
  (let* ((spec (jupyter-guess-kernelspec kernel-name))
         (kernel (jupyter--kernel-process :spec spec))
         (manager (jupyter-kernel-process-manager :kernel kernel)))
    (jupyter-start-kernel manager)
    (let ((client (jupyter-make-client manager client-class)))
      (jupyter-start-channels client)
      (jupyter--error-if-no-kernel-info client)
      ;; Un-pause the hearbeat after the kernel starts since waiting for
      ;; it to start may cause the heartbeat to think the kernel died.
      (jupyter-hb-unpause client)
      (list manager client))))

(provide 'jupyter-kernel-process-manager)

;;; jupyter-kernel-process-manager.el ends here
