#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(usocket) :silent t)
  )

; BSD 3-Clause License
; 
; Copyright (c) 2018, Sergey Polzunov
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; * Redistributions of source code must retain the above copyright notice, this
;   list of conditions and the following disclaimer.
; 
; * Redistributions in binary form must reproduce the above copyright notice,
;   this list of conditions and the following disclaimer in the documentation
;   and/or other materials provided with the distribution.
; 
; * Neither the name of the copyright holder nor the names of its
;   contributors may be used to endorse or promote products derived from
;   this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defpackage :ros.script.tcp-echo-server.3745744282
  (:use :cl))
(in-package :ros.script.tcp-echo-server.3745744282)


; This is working version of a simple TCP echo server, inspired by
; https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
; https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc
; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
;
; To execute the example, download this gist,
; install Roswell (https://github.com/roswell/roswell) and run
;
;   $ ros ./tcp-echo-server.ros
;
; To connect to a running server, run
;
;   $ telnet 127.0.0.1 8881
;
; You can find UDP server example here - https://gist.github.com/traut/648dc0d7b22fdfeae6771a5a4a19f877


(defun send-text-to-socket (text socket)
  (let ((socket-stream (usocket:socket-stream socket)))
    (format
      socket-stream
      (format nil "~a~%" text))  ; adding a line break at the end for prettiness
    (force-output socket-stream)))


(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging"
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))


(defun close-socket (socket)
  "Close a socket without raising an error if something goes wrong"
  (handler-case
      (usocket:socket-close socket)
    (error (e)
      (logger "ignoring the error that happened while trying to close socket: ~a" e)))
  (logger "socket closed"))


(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  ;; NOTE: read-line blocks until end-of-line character is received
  ;; see http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
  ;; for read-byte-at-a-time solution
  (let ((message (read-line (usocket:socket-stream client-socket))))
    (logger "got a message: ~a" message)
    (send-text-to-socket message client-socket)))


(defun run-tcp-server (host port)
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket.
  All client sockets are kept in all-sockets list."
  (let* ((master-socket (usocket:socket-listen host port :backlog 256))
         (all-sockets `(,master-socket)))
    (loop
      (loop for sock in (usocket:wait-for-input all-sockets :ready-only t)
            do (if (eq sock master-socket)
                 ; new connection initiated
                 (let ((client-socket
                         (usocket:socket-accept master-socket :element-type 'character)))
                   (push client-socket all-sockets)
                   (logger "new socket initiated: ~a" client-socket))
                 ; client socket activity
                 (handler-case
                   (process-client-socket sock)
                   (t (e)
                      (logger "error during processing ~a" e)
                      (setf all-sockets (delete sock all-sockets))
                      (close-socket sock))))))))


(defun run-server-in-thread (host port)
  "Run TCP server in a separate thread"
  (let ((thread-name (format nil "tcp-server")))
    (logger "starting tcp server in a separate thread '~a'" thread-name)
    (sb-thread:make-thread
      (lambda () (run-tcp-server host port))
      :name thread-name)))


(defun main (&rest argv)
  (declare (ignorable argv))
  (sb-thread:join-thread 
    (run-server-in-thread "0.0.0.0" 8881))
    :default nil)

;;; vim: set ft=lisp lisp:
