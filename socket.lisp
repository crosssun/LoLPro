(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(usocket) :silent t)
  )

  (defpackage :ros.script.tcp-echo-server.3745744282
    (:use :cl))
  (in-package :ros.script.tcp-echo-server.3745744282)


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

(ql:quickload "usocket")
(load "utilityLoL.lisp")

(defun create-server (text port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
	 (connection (usocket:socket-accept socket :element-type 'character)))
    (unwind-protect
	 (progn
	   (format (usocket:socket-stream connection) text)
	   (force-output (usocket:socket-stream connection)))
      (progn
	(format t "Closing sockets~%")
	(usocket:socket-close connection)
	(usocket:socket-close socket)))))

(defun create-client (port)
    (let ((socket (usocket:socket-connect "127.0.0.1" port :element-type 'character)))
      (unwind-protect
  	 (progn
  	   (usocket:wait-for-input socket)
  	   (format t "~A~%" (read-line (usocket:socket-stream socket))))
        (usocket:socket-close socket))))

(defparameter my-socket (usocket:socket-listen "127.0.0.1" 12321))
(defparameter my-connect (usocket:socket-accept my-socket :element-type 'character));on the server
(defparameter my-stream  (usocket:socket-connect "127.0.0.1" 12321 :element-type 'character)); on the CLIENT

(defmacro/g! as (tag content)
  `(format t "<~(~A~)>~A</~(~A~)>" ',tag ,content ',tag))

(defmacro/g! with (tag &rest body)
  `(progn
    (format t "~&<~(~A~)>~%" ',tag)
    ,@body
    (format t "~&</~(~A~)>~%" ',tag)))

(defun brs(&optional (n 1))
    (fresh-line)
    (dotimes (i n)
    (princ "<br>"))
    (terpri))

(defun html-file (base)
  (format nil "~(~A~).html" base))

        (defmacro/g! page (name title &rest body)
          (let ((ti (gensym)))
            `(with-open-file (*standard-output*
                              (html-file ,name)
                              :direction :output
                              :if-exists :supersede)
               (let ((,ti ,title))
                 (as title ,ti)
                 (with center
                   (as h2 (string-upcase ,ti)))
                 (brs 3)
                 ,@body))))

(defmacro/g! with-link (dest &rest body)
  `(progn (format T "<a href=\"~A\">" (html-file ,dest))
  ,@body
  (princ "</a>")))

(defun link-item (dest text)
          (princ "<li>")
          (with-link dest (princ text)
            ))

(defun button (dest text)
          (princ "[")
          (with-link dest
            (princ text))
          (format t "]~%"))

(with html (with form(princ "What's your name?")))
