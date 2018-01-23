;;; jupyter-connection.el --- Some form of connection to a Jupyter kernel -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 20 Jan 2018
;; Version: 0.0.1

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

(defgroup jupyter-connection nil
  "Some form of connection to a Jupyter kernel"
  :group 'jupyter)

(require 'jupyter-base)
(require 'eieio-base)

(defclass jupyter-connection (eieio-instance-inheritor)
  ((session
    :type jupyter-session
    :initarg :session
    :documentation "The `jupyter-session' object which holds the
 key for authenticating messages.")
   (conn-info
    :type json-plist
    :initarg :conn-info
    :documentation "The connection plist which holds the channel
 ports and other information required for connecting to a kernel.
 See
 http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files")
   (kernel-info
    :type json-plist
    :initarg :kernel-info
    :documentation "The kernel info plist of the kernel."))
  :abstract t
  :documentation "A Jupyter connection encapsulates the necessary
information of a connection to a Jupyter kernel. The necessary
information of a connection are the following:

1. A `jupyter-session' used to authenticate messages sent and
   received to a kernel.

2. The connection info plist used to initialize a connection to a
   kernel.

3. The kernel info plist after opening a connection to a
   kernel.")

(cl-defun jupyter-create-connection-info (&key
                                          (kernel-name "python")
                                          (transport "tcp")
                                          (ip "127.0.0.1")
                                          (signature-scheme "hmac-sha256")
                                          (key (jupyter-new-uuid))
                                          (hb-port 0)
                                          (stdin-port 0)
                                          (control-port 0)
                                          (shell-port 0)
                                          (iopub-port 0))
  "Create a connection info plist used to connect to a kernel.

The plist has the standard keys found in the jupyter spec. See
http://jupyter-client.readthedocs.io/en/latest/kernels.html#connection-files.
A port number of 0 for a channel means to use a randomly assigned
port for that channel."
  (unless (or (= (length key) 0)
              (equal signature-scheme "hmac-sha256"))
    (error "Only hmac-sha256 signing is currently supported"))
  (append
   (list :kernel_name kernel-name
         :transport transport
         :ip ip)
   (when (> (length key) 0)
     (list :signature_scheme signature-scheme
           :key key))
   (cl-loop
    with sock = (zmq-socket (zmq-current-context) zmq-REP)
    with addr = (concat transport "://" ip)
    for (channel . port) in (list (cons :hb_port hb-port)
                                  (cons :stdin_port stdin-port)
                                  (cons :control_port control-port)
                                  (cons :shell_port shell-port)
                                  (cons :iopub_port iopub-port))
    collect channel and
    if (= port 0) do (setq port (zmq-bind-to-random-port sock addr))
    and collect port and
    do (zmq-unbind sock (zmq-socket-get sock zmq-LAST-ENDPOINT)) else
    collect port
    finally (zmq-close sock))))

(defun jupyter-connect-endpoint (type endpoint &optional identity)
  "Create socket with type TYPE and connect it to ENDPOINT.
If IDENTITY is non-nil, it will be set as the ROUTING-ID of the
socket. The return value is the socket created."
  (let ((sock (zmq-socket (zmq-current-context) type)))
    (zmq-socket-set sock zmq-LINGER 1000)
    (when identity
      (zmq-socket-set sock zmq-ROUTING-ID identity))
    (zmq-connect sock endpoint)
    sock))

(defun jupyter-connect-channel (ctype endpoint &optional identity)
  "Create a socket based on a Jupyter channel type.
CTYPE will be mapped to a ZMQ socket type based on the plist
`jupyter-socket-types'. ENDPOINT is the endpoint the socket will
connect to and if IDENTITY is non-nil it will be set as the
ROUTING-ID of the socket. The return value is the socket
created."
  (let ((sock-type (plist-get jupyter-socket-types ctype)))
    (unless sock-type
      (error "Invalid channel type (%s)" ctype))
    (jupyter-connect-endpoint sock-type endpoint identity)))

(provide 'jupyter-connection)

;;; jupyter-connection.el ends here
