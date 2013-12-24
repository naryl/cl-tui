
(in-package cl-tui)

;;; Common stuff

(defclass frame ()
  ((parent :type (or frame boolean)
           :initarg :frame
           :initform nil)
   (children :type list
             :initform nil)
   (window :initform nil
           :documentation "Ncurses window object. Created on demand.")
   (max-w :type fixnum
          :initarg :max-w
          :initform 100500)
   (min-w :type fixnum
          :initarg :min-w
          :initform 0)
   (max-h :type fixnum
          :initarg :max-h
          :initform 100500)
   (min-h :type fixnum
          :initarg :min-h
          :initform 0)
   (weight :type fixnum
           :initarg :weight
           :initform 1)
   (border :type boolean
           :initarg :border
           :initform nil)))

(defun frame (name)
  (get name 'frame))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                       &key ((:on parent) nil)
                         left-of right-of up-of down-of
                         max-w min-w max-h min-h weight
                         border)
  (let ((directions (- 4 (count nil (list left-of right-of up-of down-of)))))
    (unless (<= directions 1)
      (error "More than one direction specified")))
  (setf parent
        (case parent
          ((t) (frame :root))
          ((nil) nil)
          (t (frame parent))))
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :parent ,parent
                               :max-w ,max-w
                               :min-w ,min-w
                               :max-h ,max-h
                               :min-h ,min-h
                               :weight ,weight
                               :border ,border))
          ,@(when parent
              ;; TODO: frame placement here
              (list
               (error "Not implemented")))))

(defgeneric render (frame)
  (:documentation "Displays the frame on screen")
  (:method :before (frame)
    (unless (slot-value frame 'window)
      (create-window frame))))

(defun create-window (frame)
  (setf (slot-value frame 'window)
        (cl-charms:newwin 10 10 10 10)))

;;; Retained frame

(defclass retained-frame (frame)
  ())

(defmethod render ((frame retained-frame))
  nil)

;;; Callback frame

(defclass callback-frame (frame)
  ((render :type function
           :initarg :render)))

(defmethod render ((frame callback-frame))
  (cl-charms:clear)
  (funcall (slot-value frame 'render)))

;;; Text frame

(defclass text-frame (frame)
  ((text :type string
         :initform "")))

(defmethod render ((frame text-frame))
  (with-slots (window text) frame
    (cl-charms:wclear window)
    (cl-charms:waddstr window text)))
