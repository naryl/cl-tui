
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render (&key h w)
  (put-text 'callback 1 1 "Try resizing the window and press space when done...")
  (put-text 'callback 2 2 "Window size: ~Ax~A" h w)
  (put-char 'callback 0 0 #\+)
  (put-char 'callback 0 (1- w) #\+)
  (put-char 'callback (1- h) 0 #\+)
  (put-char 'callback (1- h) (- w 2) #\+))

(define-frame callback (callback-frame :render 'main-render) :on t)

(defun frame ()
  (with-screen ()
    (refresh)
    (loop :until (= 32 (read-key)))))
