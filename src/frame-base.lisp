
(in-package cl-tui)

;;;; Frame

(defclass frame ()
  ((parent :type (or frame null)
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
               (ensure-ok (cl-charms:wresize window 1 1))
               (ensure-ok (cl-charms:mvwin window y x))
               (ensure-ok (cl-charms:wresize window h w)))
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
  (:method ((name symbol))
    (calculate-layout (frame name)))
  (:method ((frame frame))
    nil))

(defun frame (name)
  (when name
    (get name 'frame)))

(defsetf frame (name) (value)
  `(setf (get ,name 'frame) ,value))

(defmacro define-frame (name (type &rest frame-args)
                        &rest placement &key ((:on parent) nil))
  (remf placement :on)
  (when (eq parent t)
    (setf parent :root))
  `(progn (setf (frame ',name)
                (make-instance ',type ,@frame-args
                               :name ',name
                               :parent ',parent))
          ,@(when parent
                  (list `(add-child ',parent ',name ,@placement)))
          (when (and *running*
                     (is-frame-displayed ',name))
            (resize))
          ',name))

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

(defgeneric add-child (parent child &rest placement)
  (:documentation "Add a child to a container frame")
  (:method ((parent symbol) child &rest placement)
    (apply #'add-child (frame parent) child placement)))

(defgeneric remove-child (parent child)
  (:documentation "Remove a child from a container frame")
  (:method ((parent symbol) child)
    (remove-child (frame parent) child)))

(defun get-screen-size ()
  "Returns size of terminal screen."
  (let (rows columns)
    (cl-charms:getmaxyx cl-charms:*stdscr* rows columns)
    (list rows columns)))

(defun frame-size (&optional frame)
  "Returns the frame (Y X) size in characters. Or NIL if it's unknown yet.
Default FRAME is the whole screen."
  (if (not frame)
    (get-screen-size)
    (list (slot-value frame 'h)
          (slot-value frame 'w))))

(defun is-frame-displayed (frame)
  (cond ((eq frame *display*)
         t)
        ((eq frame nil)
         nil)
        (t
         (is-frame-displayed (slot-value (frame frame) 'parent)))))

(defun render-frame (frame)
  (render-self frame)
  (awhen (slot-value (frame frame) 'window)
    (cl-charms:wnoutrefresh it))
  (render-children frame))

(defun refresh (&optional (frame *display*))
  (cond ((is-frame-displayed frame)
         (render-frame frame)
         (cl-charms:doupdate))
        (t (cerror "Ignore" "Attempt to refresh a frame ~S which is not a child of current root ~S"
                   frame *display*)))
  nil)

(defgeneric render-self (frame)
  (:documentation "Displays the frame on screen. FRAME is the object here. Not the name.")
  (:method ((frame frame))
    nil)
  (:method ((name symbol))
    (render-self (frame name))))

(defgeneric render-children (frame)
  (:documentation "Render children for containers")
  (:method ((frame frame))
    nil)
  (:method ((name symbol))
    (render-children (frame name))))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (let+ (((h w) (frame-size)))
    (show-window *display* h w 0 0))
  (calculate-layout (frame *display*)))