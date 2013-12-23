
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

(defvar *size* nil)

(defun init-screen ()
  "Initializes the ncurses environment and starts the main-loop. Consumes the thread."
  (when *running*
    (error "Screen is already initialized"))
  (setf *running* t)
  (cl-charms:initscr)
  (cl-charms:clear)
  (cl-charms:raw)
  (cl-charms:keypad cl-charms:*stdscr* 1)
  (cl-charms:noecho)
  (setf *size* (list (cl-charms:getmaxy cl-charms:*stdscr*)
                     (cl-charms:getmaxx cl-charms:*stdscr*)))
  nil)

(defun destroy-screen ()
  (unless *running*
    (error "Screen is not initialized"))
  (cl-charms:endwin)
  (setf *running* nil)
  nil)

(defun refresh (frame)
  (render (frame frame))
  (cl-charms:refresh))

;;; Root frame definition
(setf (frame :root) (make-instance 'frame))

(defvar *display* (frame :root))