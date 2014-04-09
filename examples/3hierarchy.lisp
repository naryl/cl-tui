

(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render2 (&key h w)
  (put-char 'callback2 2 2 #\+)
  (put-char 'callback2 2 (- w 3) #\+)
  (put-char 'callback2 (- h 3) 2 #\+)
  (put-char 'callback2 (- h 3) (- w 3) #\+))

(define-frame callback (container-frame) :on :root)

(define-frame callback2 (callback-frame :render 'main-render2) :on callback)

(defun hierarchy ()
  (cl-tui:with-screen ()
    (refresh)
    (loop :until (= 32 (read-key)))))
