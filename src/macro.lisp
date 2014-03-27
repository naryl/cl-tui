
(in-package cl-tui)

(defvar *delayed-init* nil)

(defmacro when-running (&body body)
  (with-gensyms (func)
    `(labels ((,func () ,@body))
       (when *running*
         (,func))
       (push #',func *delayed-init*)
       nil)))

(defun do-delayed-init ()
  (mapc #'funcall (nreverse *delayed-init*)))
