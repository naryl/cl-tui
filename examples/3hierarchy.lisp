
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun main-render (&key frame h w)
  (put-char frame 2 2 #\+)
  (put-char frame 2 (- w 3) #\+)
  (put-char frame (- h 3) 2 #\+)
  (put-char frame (- h 3) (- w 3) #\+))

;; Container frame can have several frames on top of it. Currently
;; each child occupies equal space either horizontally or vertically
(define-frame container (container-frame) :on :root)

;; Placing the frame on the container instead of :root
(define-frame callback (simple-frame :render 'main-render) :on container)

(defun start ()
  (cl-tui:with-screen ()
    (refresh)
    (loop :until (eql #\Space (read-key)))))
