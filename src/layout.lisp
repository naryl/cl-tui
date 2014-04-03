
(in-package cl-tui)

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

(defun ensure-window (frame h w x1 y1 x2 y2)
  (with-slots (window) frame
    (if window
      (if (not (slot-value window 'window))
        (when init-ncurses-window?
          (setf (slot-value window 'window) (cl-charms:newwin h w y1 x1)))
        (progn (cl-charms:mvwin (slot-value window 'window) y1 x1)
               (cl-charms:wresize (slot-value window 'window) h w)
               (setf (slot-value window 'x1) x1
                     (slot-value window 'y1) y1
                     (slot-value window 'x2) x2
                     (slot-value window 'y2) y2)))
      (setf window
            (make-instance 'Window
                           :x1 x1
                           :y1 y1
                           :x2 x2
                           :y2 y2
                           ;; Implement this with a generic. Containers should forbid a
                           ;; lot of things, not just ncurses windows
                           :window (unless (eq (type-of frame) 'container-frame)
                                     (cl-charms:newwin h w y1 x1)))))))

;;; TODO Implement weights support
;;; TODO Implement max-* and min-* support
(defun calculate-layout (frame)
  "Builds layout tree for given frame."
  (with-slots (children parent split-type) frame
    (let+ (((rows columns) (frame-size parent))
           (limit (case split-type
                    (:vertical columns)
                    (:horizontal rows))))
      (ensure-window frame rows columns 0 0 (1- columns) (1- rows))
      (when children
        (loop
          with step = (ceiling (/ limit (length children)))
          for child in children
          for shift from 0 upto limit by step
          doing
             (calculate-layout child)
             (progn
               (with-slots (min-rows min-columns
                            max-rows max-columns
                            weight window)
                   child
                 (let* ((x1 (case split-type
                              (:vertical shift)
                              (:horizontal 0)))
                        (y1 (case split-type
                              (:vertical 0)
                              (:horizontal shift)))
                        (ncolumns (case split-type
                                    (:vertical step)
                                    (:horizontal columns)))
                        (nrows (case split-type
                                 (:vertical rows)
                                 (:horizontal step)))
                        (x2 (1- (+ x1 ncolumns)))
                        (norm-x2 (if (<= columns x2)
                                     (1- columns)
                                     x2))
                        (y2 (1- (+ y1 nrows)))
                        (norm-y2 (if (<= rows y2)
                                     (1- rows)
                                     y2)))
                   (ensure-window child nrows ncolumns x1 y1 norm-x2 norm-y2)))))))
    t))

(defun refresh (&optional (frame *display*))
  (labels ((is-frame-displayed (frame)
             (cond ((eq frame *display*)
                    t)
                   ((eq frame nil)
                    nil)
                   (t
                    (is-frame-displayed (slot-value (frame frame) 'parent)))))
           (render-tree (frame)
             (when frame
               (with-slots (window children) frame
                 (if children
                   (mapcar #'render-tree children)
                   (when (slot-value window 'window)
                     (cl-charms:wnoutrefresh (slot-value window 'window))))))))
    (cond ((is-frame-displayed frame)
           (render-tree (frame frame))
           (cl-charms:doupdate))
          (t (cerror "Attempt to refresh a frame ~S which is not a child of current root ~S"
                     frame *display*)))
    nil))

(defgeneric render (frame)
  (:documentation "Displays the frame on screen. FRAME is the object here. Not the name"))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (calculate-layout (frame *display*)))
