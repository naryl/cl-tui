
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

;;; Colors can be initialized before init-screen
(defvar rainbow (list (color 1000 0 0)
                      (color 1000 1000 0)
                      (color 0 1000 0)
                      (color 0 1000 1000)
                      (color 0 0 1000)
                      (color 1000 0 1000)
                      ))
(defvar rainbow-pairs (loop :for i :below 6
                         :collect (color-pair
                                   (elt rainbow (mod i 6))
                                   (elt rainbow (mod (1+ i) 6)))))

(defun gray (intensity)
  (color intensity intensity intensity))

(defun main-render (&key)
  (dotimes (i 36)
    (with-attributes ((:color (elt rainbow-pairs (mod i 6)))) 'callback
      (put-char 'callback 1 (1+ i) #\X)))
  (dotimes (i 20)
    (let* ((intensity (* 50 i))
           (fg (gray (+ 50 intensity)))
           (bg (gray intensity)))
      (with-attributes ((:color (color-pair fg bg))) 'callback
        (dotimes (j 3)
          (put-char 'callback (+ 2 j) (1+ i) #\X))))))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(defun colors ()
  (with-screen (:colors)
    (refresh)
    (read-key)))
