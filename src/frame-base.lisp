
(in-package cl-tui)

;;;; Frame

(defclass frame ()
  ((max-rows :initarg :max-rows)
   (max-columns :initarg :max-columns)
   (min-rows :initarg :min-rows)
   (min-columns :initarg :min-columns)
   (weight :initarg :weight)
   (parent :type (or frame null)
           :initarg :parent
           :initform nil)
   ;; Actual coordinates used by the layouter
   (h) (w) (y) (x)
   (window :initform nil
           :documentation "Ncurses window object. Created on demand.")))

(defun hide-window (frame)
  (with-slots (window) (frame frame)
    (cl-charms:delwin window)
    (setf window nil)))

(defun show-window (frame nh nw ny nx)
  (let ((frame (frame frame)))
    (with-slots (h w y x window) frame
      (setf h nh
            w nw
            y ny
            x nx)
      (when (frame-drawable-p frame)
        (cond (window
               (cl-charms:mvwin window y x)
               (cl-charms:wresize window h w))
              (t
               (setf window (cl-charms:newwin h w y x))))))))

(defgeneric frame-drawable-p (frame)
  (:documentation "Returns whether instances of this frame can be drawed on. Otherwise no
  ncurses windows will be created")
  (:method ((frame frame))
    nil))

(defgeneric calculate-layout (frame)
  (:documentation "The frame should have actual X Y H and W when this generic is called.

  It sets the frame's children's coordinates and sizes to actual values and recursively
  calls itself on them.

  The default method does nothing and should be overloaded in container frames.")
  (:method ((frame frame))
    nil))

(defun frame (name)
  (when name
    (get name 'frame)))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                       &key (parent nil)
                         (split-type :none) position
                         max-columns min-columns max-rows min-rows weight)
  (unless (find split-type '(:vertical :horizontal :none))
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

(defun destroy-frame (name)
  (let ((frame (frame name)))
    (awhen frame
      (with-slots (window children parent) it
        (when children
          (mapc #'destroy-frame children))
        (when window
          (hide-window name))
        (remprop name 'frame)
        t))))

(defun add-child (parent child position)
  (with-slots (children) parent
    (if (not (numberp position))
      (push child children)
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

;;; TODO Implement weights support
;;; TODO Implement max-* and min-* support

(defun refresh (&optional (frame *display*))
  (labels ((is-frame-displayed (frame)
             (cond ((eq frame *display*)
                    t)
                   ((eq frame nil)
                    nil)
                   (t
                    (is-frame-displayed (slot-value (frame frame) 'parent))))))
    (cond ((is-frame-displayed frame)
           (render (frame frame))
           (cl-charms:doupdate))
          (t (cerror "Attempt to refresh a frame ~S which is not a child of current root ~S"
                     frame *display*)))
    nil))

(defgeneric render (frame)
  (:documentation "Displays the frame on screen. FRAME is the object here. Not the name")
  (:method :after ((frame frame))
    (with-slots (window) frame
      (when window
        (cl-charms:wnoutrefresh window)))))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (let+ (((h w) (frame-size)))
    (show-window *display* h w 0 0))
  (calculate-layout (frame *display*)))
