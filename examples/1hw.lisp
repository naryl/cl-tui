
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun start ()
  (with-screen ()
    (put-text :root 0 0 "Hello world!")
    (refresh)
    (read-key)))
