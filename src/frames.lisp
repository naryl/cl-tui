
(in-package cl-tui)

;;; Common stuff

;;;; FRAME TYPES

;;; Dummy frame for holding other frames

(defclass container-frame (frame)
  ((children :initform nil
             :documentation "Alist of frame names and any placement arguments.")
   (split-type :type (member :none :vertical :horizontal)
               :initform :vertical)))

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
                          (:vertical h)
                          (:horizontal w))))
             (loop
                with step = (truncate limit (length children))
                for child in children
                for shift from 0 upto limit by step
                doing
                  (let ((x (+ x (case split-type
                                  (:vertical 0)
                                  (:horizontal shift))))
                        (y (+ y (case split-type
                                  (:vertical shift)
                                  (:horizontal 0))))
                        (h (case split-type
                             (:vertical step)
                             (:horizontal h)))
                        (w (case split-type
                             (:vertical w)
                             (:horizontal step))))
                    (show-window (car child) h w y x)
                    (calculate-layout (car child)))))))))

(defmethod render-children ((frame container-frame))
  (mapcar (compose #'render-frame #'car)
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
           :initform nil)))

(defmethod initialize-instance ((frame callback-frame) &key name render)
  (call-next-method)
  (when render
    (setf (slot-value frame 'render)
          (lambda ()
            (funcall render
                     :frame name
                     :h (cl-charms:getmaxy (slot-value frame 'window))
                     :w (cl-charms:getmaxx (slot-value frame 'window))
                     :allow-other-keys t)))))

(defmethod render-self ((frame callback-frame))
  (with-slots (render window) frame
    (cl-charms:werase window)
    (when render
      (funcall render))))

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
