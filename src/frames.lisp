
(in-package cl-tui)

;;; Common stuff

;;;; FRAME TYPES

;;; Dummy frame for holding other frames

(defclass container-frame (frame)
  ((children :initform nil
             :documentation "Alist of frame names and any placement arguments.")
   (split-type :type (member :none :vertical :horizontal)
               :initform :none)))

(defmethod add-child ((frame container-frame) child &rest placement)
  (with-slots (children) frame
    (deletef children child :key #'car)
    (push (list* child placement) children)))

(defmethod remove-child ((frame container-frame) child)
  (with-slots (children) frame
    (setf (slot-value (frame child) 'parent) nil)
    (deletef children child :key #'car)))

(defmethod calculate-layout ((frame container-frame))
  (with-slots (h w y x children split-type) frame
    (cond ((null children)
           nil)
          ((= 1 (length children))
           (show-window (caar children) h w y x)
           (calculate-layout (caar children)))
          (t
           (let ((limit (ecase split-type
                          (:vertical w)
                          (:horizontal h))))
             (loop
                with step = (ceiling (/ limit (length children)))
                for child in children
                for shift from 0 upto limit by step
                doing
                  (let ((x (+ x (case split-type
                                  (:vertical shift)
                                  (:horizontal 0))))
                        (y (+ y (case split-type
                                  (:vertical 0)
                                  (:horizontal shift))))
                        (h (case split-type
                             (:vertical step)
                             (:horizontal h)))
                        (w (case split-type
                             (:vertical w)
                             (:horizontal step))))
                    (show-window (car child) h w y x)
                    (calculate-layout (car child)))))))))

(defmethod render-children ((frame container-frame))
  (mapcar (compose #'render-frame #'frame #'car)
          (slot-value frame 'children)))

;;; Canvas frame superclass (for frames allowed to use simple drawing functions)

(defclass canvas-frame (frame) ())

(defmethod frame-drawable-p ((frame canvas-frame))
  t)

;;; Retained frame

(defclass retained-frame (canvas-frame)
  ())

;;; Callback frame

(defclass callback-frame (canvas-frame)
  ((render :type function
           :initform nil
           :initarg :render)))

(defmethod render-self ((frame callback-frame))
  (with-slots (render window) frame
    (cl-charms:wclear window)
    (when render
      (funcall render
               :h (cl-charms:getmaxy window)
               :w (cl-charms:getmaxx window)
               :allow-other-keys t))))

;;; Text frame

(defclass text-frame (frame)
  ((text :type string
         :initform "")))

(defmethod render-self ((frame text-frame))
  (with-slots (window text) frame
    (cl-charms:wclear (slot-value window 'window))
    (loop :for i :from 0
       :for line :in text
       :do (cl-charms:mvwaddstr window i 0 text))))
