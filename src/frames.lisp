
(in-package cl-tui)

;;; Common stuff

(defclass Window ()
  ((x1 :initarg :x1)
   (y1 :initarg :y1)
   (x2 :initarg :x2)
   (y2 :initarg :y2)
   (window :initarg :window
           :initform nil
           :documentation "Ncurses window object. Created on demand.")))

(defclass Frame ()
  ((name :initarg :name)
   (max-rows :initarg :max-rows)
   (max-columns :initarg :max-columns)
   (min-rows :initarg :min-rows)
   (min-columns :initarg :min-columns)
   (weight :initarg :weight)
   (parent :type (or frame null)
           :initarg :parent
           :initform nil)
   (children :type (or null layout)
             :initform nil)
   (split-type :type (member :vertical :horizontal)
               :initarg :split-type)
   (window :initform nil
           :documentation "Describes frame position and size. Initialized by layouter.")))

(defun frame (name)
  (when name
    (get name 'frame)))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                       &key (parent nil)
                         split-type position
                         max-columns min-columns max-rows min-rows weight)
  (unless (find split-type '(:vertical :horizontal))
    (error "Unknown split type: ~S" split-type))
  (when (eq parent t)
    (setf parent :root))
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :name ',name
                               :parent (frame ',parent)
                               :min-columns ,min-columns
                               :max-columns ,max-columns
                               :min-rows ,min-rows
                               :max-rows ,max-rows
                               :weight ,weight
                               :split-type ,split-type))
          ,@(when parent
                  (list `(add-child (frame ',parent) ',name ',position)))
          ',name
          (when *running* (resize))))

(defun destroy-window (window)
  (with-slots (window) window
    (when window
      (cl-charms:delwin window))))

(defgeneric destroy-frame (frame))

(defmethod destroy-frame ((name symbol))
  (when name
    (destroy-frame (frame name))))

(defmethod destroy-frame ((frame Frame))
  (awhen frame
    (with-slots (window children parent) it
      (when children
        (mapc #'destroy-frame children))
      (when window
        (destroy-window window))
      (remprop (slot-value frame 'name) 'frame)
      t)))

(defun add-child (parent child position)
  (with-slots (children) parent
    (if (not (numberp position))
      (push (frame child) children)
      (setf children (append (subseq children 0 (1+ position))
                             (list (frame child))
                             (subseq children (1+ position)))))
    (setf (slot-value (frame child) 'parent) parent)))

(defun remove-child (parent child)
  (with-slots (children) parent
    (setf children (remove child children))))

(defun get-screen-size ()
  "Returns size of terminal screen."
  (let (rows columns)
    (cl-charms:getmaxyx cl-charms:*stdscr* rows columns)
    (list columns rows)))

(defun frame-size (&optional frame)
  "Returns the frame (Y X) size in characters. Or NIL if it's unknown yet.
Default FRAME is the whole screen."
  (if (not frame)
    (get-screen-size)
    (let ((window (slot-value frame 'window)))
      (when window
        (with-slots (x1 y1 x2 y2) window
          (list (1+ (- x2 x1))
                (1+ (- y2 y1))))))))

;;;; FRAME TYPES

;;; Canvas frame superclass (for frames allowed to use simple drawing functions)

(defclass canvas-frame (frame) ())

;;; Retained frame

(defclass retained-frame (canvas-frame)
  ())

;;; Callback frame

(defclass callback-frame (canvas-frame)
  ((render :type function
           :initform nil
           :initarg :render)))

;;; Text frame

(defclass text-frame (frame)
  ((text :type string
         :initform "")))
