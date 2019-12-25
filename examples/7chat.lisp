
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(define-frame :root (container-frame :split-type :vertical))
(define-frame log (log-frame) :on :root)
;(define-frame input (edit-frame) :on :root)

(defun finish-input ()
  nil)

(defun chat ()
  (with-screen ()
    (with-attributes ((:color (color 0 1000 0) (color 0 0 0)))
        'log
      (loop
         (refresh)
         (let ((key (read-key)))
           (case key
             (#\Return (finish-input))
             (t (handle-key input))))))))
