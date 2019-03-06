(defun repl ()
  (let ((server (socket:socket-server 9000 :interface "127.0.0.1")))
       (format t "~&Waiting for a connection on ~S:~D~%"
               (socket:socket-server-host server) (socket:socket-server-port server))
       (unwind-protect
         ;; infinite loop, terminate with Control+C
         (loop (with-open-stream (socket (socket:socket-accept server))
                                 (multiple-value-bind (local-host local-port) (socket:socket-stream-local socket)
                                                      (multiple-value-bind (remote-host remote-port) (socket:socket-stream-peer socket)
                                                                           (format t "~&Connection: ~S:~D -- ~S:~D~%"
                                                                                   remote-host remote-port local-host local-port)))
                                 ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
                                 (loop (when (eq :eof (socket:socket-status (cons socket :input))) (return))
                                       (prin1 (repl-eval (read socket)) socket)
                                       ;; flush everything left in socket
                                       (loop :for c = (read-char-no-hang socket nil nil) :while c))))
         ;; make sure server is closed
         (socket:socket-server-close server))))

(defun repl-eval (readstring)
  (handler-case (eval readstring)
    (error (c) (apply #'format nil
                      (concatenate 'string "*** - " (simple-condition-format-control c))
                      (simple-condition-format-arguments c)))))

(repl)
