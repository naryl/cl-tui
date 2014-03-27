
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

; Colors can be initialized before init-screen
(defvar colors (list (make-color 1000 0 0)
                     (make-color 1000 1000 0)
                     (make-color 0 1000 0)
                     (make-color 0 1000 1000)
                     (make-color 0 0 1000)
                     (make-color 1000 0 1000)
                     ))

(init-screen :colors)

(defun main-render ()
  (let ((pairs (loop :for i :below 6
                  :collect (make-color-pair (mod i 6) (mod (1+ i) 6)))))
    (loop :for i :below 36
       :do (with-attributes ((:color (elt pairs (mod i 6)))) 'callback
             (put-char 'callback (1+ i) 1 #\X)))))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(refresh)

(read-key)

(destroy-screen)
