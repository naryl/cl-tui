
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(define-frame log (log-frame) :on :root)

(defun log-test ()
  (with-screen (:colors)
    (with-attributes ((:color (color 0 600 600) (color 800 800 0)))
        'log
      (append-line 'log "Press C-q to quit"))
    (with-attributes ((:color (color 0 1000 0) (color 0 0 0)))
        'log
      (loop
         (refresh)
         (let ((key (read-key)))
           (case key
             (#\Dc1 (return))
             (t (append-line 'log "~A" key))))))))
