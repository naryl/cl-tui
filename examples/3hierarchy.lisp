

(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render2 (&key x y)
  (put-char 'callback 2 2 #\+)
  (put-char 'callback 2 (- x 3) #\+)
  (put-char 'callback (- y 3) 2 #\+)
  (put-char 'callback (- y 3) (- x 3) #\+))

(define-frame callback (callback-frame)
              :parent :root
              :split-type :horizontal)

(define-frame callback2 (callback-frame :render 'main-render2)
              :parent callback
              :split-type :vertical)

(defun hierarchy ()
  (cl-tui:with-screen ()
    (refresh)
    (read-key)))
