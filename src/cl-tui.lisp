
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

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
  (resize)
  nil)

(defun destroy-screen ()
  "Return terminal to default mode"
  (unless *running*
    (error "Screen is not initialized"))
  (cl-charms:endwin)
  (setf *running* nil)
  nil)

;;; Root frame definition
(sunless (frame :root)
  (setf it (make-instance 'retained-frame)))

(defvar *display* :root)

(defun display (&optional (frame :root))
  "Set the root frame. Only it and its children will be displayed.
  Default is :ROOT frame."
  (setf *display* frame)
  (resize)
  (refresh))
