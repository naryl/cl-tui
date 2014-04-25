
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(define-frame log (log-frame) :on :root)

(defun log-test ()
  (with-screen ()
    (append-line 'log "Press C-q to quit")
    (loop
       (refresh)
       (let ((key (read-key)))
         (case key
           (#\Dc1 (return))
           (t (append-line 'log (string key))))))))
