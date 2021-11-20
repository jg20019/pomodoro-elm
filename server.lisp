(ql:quickload 'hunchentoot)


(defparameter *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(setf (hunchentoot:acceptor-document-root *acceptor*) ".")

(defun start ()
  (hunchentoot:start *acceptor*))

(start)
