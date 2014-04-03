
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render (&key x y)
  (put-text 'callback 1 1 "Try resizing the window and press any key when done...")
  (put-char 'callback 0 0 #\+)
  (put-char 'callback 0 (1- x) #\+)
  (put-char 'callback (1- y) 0 #\+)
  (put-char 'callback (1- y) (1- x) #\+))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(defun frame ()
  (with-screen ()
    (refresh)
    (read-key)))
