
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

; Colors can be initialized before init-screen
(defvar rainbow (list (make-color 1000 0 0)
                      (make-color 1000 1000 0)
                      (make-color 0 1000 0)
                      (make-color 0 1000 1000)
                      (make-color 0 0 1000)
                      (make-color 1000 0 1000)
                      ))

(defvar intensities (loop :for i :to 1000 :by 50
                       :collect i))
(defvar gradient (mapcar #'make-color intensities intensities intensities))

(init-screen :colors)

(defun main-render ()
  (let ((pairs (loop :for i :below 6
                  :collect (make-color-pair
                            (elt rainbow (mod i 6))
                            (elt rainbow (mod (1+ i) 6))))))
    (dotimes (i 36)
      (with-attributes ((:color (elt pairs (mod i 6)))) 'callback
        (put-char 'callback (1+ i) 1 #\X))))
  (let ((pairs (loop :for i :below 20
                  :collect (make-color-pair
                            (elt gradient (1+ i))
                            (elt gradient i)))))
    (dotimes (i 20)
      (with-attributes ((:color (elt pairs i))) 'callback
        (dotimes (j 3)
          (put-char 'callback (1+ i) (+ 2 j) #\X))))))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(refresh)

(read-key)

(destroy-screen)
