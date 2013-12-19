
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

(defvar *size* nil)

(defun init-screen ()
  "Initializes the ncurses environment and starts the main-loop. Consumes the thread."
  (when *running*
    (error "Screen is already initialized"))
  (setf *running* t)
  (initscr)
  (clear)
  (raw)
  (keypad *stdscr* 1)
  (noecho)
  (setf *size* (list (getmaxy *stdscr*) (getmaxx *stdscr*)))
  nil)

(defun destroy-screen ()
  (unless *running*
    (error "Screen is not initialized"))
  (endwin)
  (setf *running* nil)
  nil)
