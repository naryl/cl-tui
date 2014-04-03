
(in-package cl-tui)

;;; Common stuff

;;;; FRAME TYPES

;;; Dummy frame for holding other frames

(defclass container-frame (frame) ())

;;; Canvas frame superclass (for frames allowed to use simple drawing functions)

(defclass canvas-frame (frame) ())

;;; Retained frame

(defclass retained-frame (canvas-frame)
  ())

(defmethod render ((frame retained-frame))
  nil)

;;; Callback frame

(defclass callback-frame (canvas-frame)
  ((render :type function
           :initform nil
           :initarg :render)))

(defmethod render ((frame callback-frame))
  (with-slots (render window) frame
    (with-slots (window) window
      (cl-charms:wclear window)
      (when render
        (funcall render
                 :y (cl-charms:getmaxy window)
                 :x (cl-charms:getmaxx window)
                 :allow-other-keys t)))))

;;; Text frame

(defclass text-frame (frame)
  ((text :type string
         :initform "")))

(defmethod render ((frame text-frame))
  (with-slots (window text) frame
    (cl-charms:wclear (slot-value window 'window))
    (loop :for i :from 0
       :for line :in text
       :do (cl-charms:mvwaddstr window i 0 text))))
