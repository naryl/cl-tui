
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
                         max-w min-w max-h min-h weight
                         border)
  (let ((directions (- 4 (count nil (list left-of right-of up-of down-of)))))
    (unless (<= directions 1)
      (error "More than one direction specified")))
  (when (eq parent t)
    (setf parent :root))
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :parent (frame ',parent)))
          ,@(when parent
                  (list `(add-child (frame ',parent) (frame ',name))))
          ',name))

(defun add-child (parent child)
  (with-slots (children) parent
    (unless children
      (setf children (make-layout)))
    (layout-insert (slot-value parent 'children) child)))

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
                    (is-frame-displayed (slot-value frame 'parent)))))
           (render-tree (frame-name)
             (let ((frame (frame frame-name)))
               (render frame)
               (with-slots (window children) frame
                 (cl-charms:wnoutrefresh window)
                 (mapcar #'render-tree children)))))
    (cond ((is-frame-displayed frame)
           (render-tree frame)
           (cl-charms:doupdate))
          (t (cerror "Attempt to refresh a frame ~S
which is not a child of current root ~S" frame *display*)))))

(defgeneric render (frame)
  (:documentation "Displays the frame on screen")
  (:method :before (frame)
    (with-slots (window border) frame
      (when border
        (apply #'cl-charms:box window
               (mapcar #'char-code (list #\| #\-)))))))

(defun subwindow-p (window)
  (= -1 (cl-charms:getparx window)))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (labels ((delete-windows (frame)
             (with-slots (children window) frame
               (mapcar #'delete-windows children)
               (cl-charms:delwin window)
               (setf window nil))))
    (let+ (((h w) (frame-size)))
      (with-slots (window children) (frame *display*)
        (when children
          (apply #'recalculate-layout children (frame-size)))
        (when window
          (cond ((subwindow-p window)
                 (delete-windows window))
                (t (cl-charms:mvwin window 0 0)
                   (cl-charms:wresize window h w))))
        (unless window
          (setf window (cl-charms:newwin h w 0 0)))))))

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
           :initarg :render)))

(defmethod render ((frame callback-frame))
  (with-slots (render window) frame
    (cl-charms:wclear window)
    (funcall render)))

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
