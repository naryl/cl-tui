
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render (&key w h)
  (put-text 'callback 1 1 "Try resizing the window and press any key when done...")
  (put-char 'callback 0 0 #\+)
  (put-char 'callback 0 (1- w) #\+)
  (put-char 'callback (1- h) 0 #\+)
  (put-char 'callback (1- h) (1- w) #\+))

(define-frame callback (callback-frame :render 'main-render) :on t)

(defun frame ()
  (with-screen ()
    (refresh)
    (read-key)))
