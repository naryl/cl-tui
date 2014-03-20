
(in-package cl-tui)

;;; Common stuff

(defclass frame ()
  ((parent :type (or frame null)
           :initarg :parent
           :initform nil)
   (children :type (or null layout)
             :initform nil)
   (window :initform nil
           :documentation "Ncurses window object. Created on demand.")))

(defun frame (name)
  (when name
    (get name 'frame)))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                       &key ((:on parent) nil)
                         left-of right-of up-of down-of
                         max-w min-w max-h min-h weight)
  (let ((directions (- 4 (count nil (list left-of right-of up-of down-of)))))
    (unless (<= directions 1)
      (error "More than one direction specified")))
  (when (eq parent t)
    (setf parent :root))
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :parent (frame ',parent)))
          ,@(when parent
                  (list `(add-child (frame ',parent) ',name)))
          ',name
          (when *running* (resize))))

(defun add-child (parent child)
  (with-slots (children) parent
    (unless children
      (setf children (make-layout)))
    (layout-insert children child)
    (setf (slot-value (frame child) 'parent) parent)))

(defun remove-child (parent child)
  (with-slots (children) parent
    (layout-remove children child)))

(defun frame-size (&optional frame)
  "Returns the frame (Y X) size in characters. Or NIL if it's unknown yet.
Default FRAME is the whole screen."
  (let ((window (if frame
                    (slot-value (frame frame) 'window)
                    cl-charms:*stdscr*)))
    (when window
      (list (cl-charms:getmaxy window)
            (cl-charms:getmaxx window)))))

(defun refresh (&optional (frame *display*))
  (labels ((is-frame-displayed (frame)
             (cond ((eq frame *display*)
                    t)
                   ((eq frame nil)
                    nil)
                   (t
                    (is-frame-displayed (slot-value (frame frame) 'parent)))))
           (render-tree (frame)
             (render frame)
             (with-slots (window children) frame
               (cl-charms:wnoutrefresh window)
               (when children
                 (mapcar #'render-tree
                         (mapcar #'frame
                                 (mapcar #'layout-cell-frame
                                         (layout-cells children))))))))
    (cond ((is-frame-displayed frame)
           (render-tree (frame frame))
           (cl-charms:doupdate))
          (t (cerror "Attempt to refresh a frame ~S
which is not a child of current root ~S" frame *display*)))
    nil))

(defgeneric render (frame)
  (:documentation "Displays the frame on screen. FRAME is the object here. Not the name")
  (:method :before (frame)
    nil))

(defun subwindow-p (window)
  (= -1 (cl-charms:getparx window)))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (let+ (((h w) (frame-size)))
    (with-slots (window children) (frame *display*)
      (when children
        (apply #'recalculate-layout children (frame-size)))
      (ensure-window *display* h w 0 0)
      )))

(defun ensure-window (frame h w x y)
  (with-slots (window) (frame frame)
    (cond (window
           (cl-charms:mvwin window y x)
           (cl-charms:wresize window h w))
          (t
           (setf window (cl-charms:newwin h w y x))))))

;;;; FRAME TYPES

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
    (cl-charms:wclear window)
    (when render
      (funcall render))))

;;; Text frame

(defclass text-frame (frame)
  ((text :type string
         :initform "")))

(defmethod render ((frame text-frame))
  (with-slots (window text) frame
    (cl-charms:wclear window)
    (loop :for i :from 0
       :for line :in text
       :do (cl-charms:mvwaddstr window i 0 text))))
