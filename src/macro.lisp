
(in-package cl-tui)

(defvar *delayed-init* nil)

(defmacro when-running (&body body)
  (with-gensyms (func)
    `(labels ((,func () ,@body))
       (if *running*
           (,func)
           (push #',func *delayed-init*))
       (values))))

(defun do-delayed-init ()
  (mapc #'funcall (nreverse *delayed-init*))
  (setf *delayed-init* nil))

(defmacro ensure-ok (form)
  (with-gensyms (result)
    `(let ((,result ,form))
       (unless (= ,result charms/ll:ok)
         (cerror "Ignore" "Something went wrong in ~S~%Result: ~S" ',form ,result)))))
