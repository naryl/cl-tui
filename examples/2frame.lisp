
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(init-screen)

(defun main-render ()
          (destructuring-bind (x y)
              (frame-size 'callback)
            (put-text 'callback 1 1 "Try resizing the window and press any key when done...")
          (put-char 'callback 0 0 #\+)
          (put-char 'callback 0 (1- x) #\+)
          (put-char 'callback (1- y) 0 #\+)
          (put-char 'callback (1- y) (1- x) #\+)))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(refresh)

(read-key)

(destroy-screen)
