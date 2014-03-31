
(in-package cl-tui)

(defun ensure-window (frame h w x1 y1 x2 y2 &optional (init-ncurses-window? t))
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
                           :window (when init-ncurses-window?
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
      (ensure-window frame rows columns 0 0 (1- columns) (1- rows) (when children nil))
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
  (:documentation "Displays the frame on screen. FRAME is the object here. Not the name")
  (:method :before (frame)
    nil))

(defun resize ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  (calculate-layout (frame *display*)))

(defmethod render ((frame retained-frame))
  nil)

(defmethod render ((frame callback-frame))
  (with-slots (render window) frame
    (cl-charms:wclear (slot-value window 'window))
    (when render
      (funcall render))))

(defmethod render ((frame text-frame))
  (with-slots (window text) frame
    (cl-charms:wclear (slot-value window 'window))
    (loop :for i :from 0
       :for line :in text
       :do (cl-charms:mvwaddstr window i 0 text))))
