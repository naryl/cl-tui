
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

(defvar *size* nil)

(defun init-screen ()
  "Initializes the ncurses environment"
  (when *running*
    (error "Screen is already initialized"))
  (setf *running* t)
  (cl-charms:initscr)
  (cl-charms:clear)
  (cl-charms:raw)
  (cl-charms:keypad cl-charms:*stdscr* 1)
  (cl-charms:noecho)
  (cl-charms:curs-set 0)
  (setf *size* (list (cl-charms:getmaxy cl-charms:*stdscr*)
                     (cl-charms:getmaxx cl-charms:*stdscr*)))
  nil)

(defun destroy-screen ()
  "Return terminal to default mode"
  (unless *running*
    (error "Screen is not initialized"))
  (cl-charms:endwin)
  (setf *running* nil)
  nil)

(defun refresh (&optional (frame *display*))
  (labels ((render-tree (frame)
             (let ((frame (frame frame)))
               (render frame)
               (mapcar #'render-tree (slot-value frame 'children)))))
    (render-tree frame)))

;;; Root frame definition
(sunless (frame :root)
  (setf it (make-instance 'retained-frame)))

(defvar *display* :root)

(defun display (&optional (frame :root))
  "Set the root frame. Only it and its children will be displayed.
  Default is :ROOT frame."
  (setf *display* frame)
  (resize))

(defun resize ()
  (ensure-window-sizes)
  (refresh))

(defun ensure-window-sizes ()
  "Makes sure *DISPLAY* frame and all its children have proper place on the screen"
  ;; TODO:
  nil)
