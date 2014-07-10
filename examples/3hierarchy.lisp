

(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render2 (&key frame h w)
  (put-char frame 2 2 #\+)
  (put-char frame 2 (- w 3) #\+)
  (put-char frame (- h 3) 2 #\+)
  (put-char frame (- h 3) (- w 3) #\+))

(define-frame callback (container-frame) :on :root)

(define-frame callback2 (callback-frame :render 'main-render2) :on callback)

(defun hierarchy ()
  (cl-tui:with-screen ()
    (refresh)
    (loop :until (= 32 (read-key)))))
