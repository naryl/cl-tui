
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

;;; This is just a trivial test script for Travis. Does almost nothing.

(defun start ()
  (with-screen ()
    (put-text :root 0 0 "+")
    (refresh)))
