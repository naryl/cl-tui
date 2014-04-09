
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

;;; Colors can be initialized before init-screen
(defvar rainbow (list (make-color 1000 0 0)
                      (make-color 1000 1000 0)
                      (make-color 0 1000 0)
                      (make-color 0 1000 1000)
                      (make-color 0 0 1000)
                      (make-color 1000 0 1000)
                      ))
(defvar rainbow-pairs (loop :for i :below 6
                         :collect (make-color-pair
                                   (elt rainbow (mod i 6))
                                   (elt rainbow (mod (1+ i) 6)))))

(defvar intensities (loop :for i :to 1000 :by 50
                       :collect i))
(defvar gradient (mapcar #'make-color intensities intensities intensities))
(defvar gradient-pairs (loop :for i :below 20
                          :collect (make-color-pair
                                    (elt gradient (1+ i))
                                    (elt gradient i))))
(defun main-render (&key)
      (dotimes (i 36)
        (with-attributes ((:color (elt rainbow-pairs (mod i 6)))) 'callback
          (put-char 'callback 1 (1+ i) #\X)))
      (dotimes (i 20)
        (with-attributes ((:color (elt gradient-pairs i))) 'callback
          (dotimes (j 3)
            (put-char 'callback (+ 2 j) (1+ i) #\X)))))

(define-frame callback (callback-frame :render 'main-render) :on :root)

(defun colors ()
  (with-screen (:colors)
    (refresh)
    (read-key)))
