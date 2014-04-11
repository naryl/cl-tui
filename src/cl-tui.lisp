
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

(defun init-screen (&rest arguments)
  "Initializes the ncurses environment"
  (when *running*
    (error "Screen is already initialized"))
  (setf *running* t)
  (cl-charms:initscr)
  (let ((arguments (or arguments '(:raw :noecho))))
    (dolist (argument arguments)
      (case argument
        (:echo (cl-charms:echo))
        (:noecho (cl-charms:noecho))
        (:raw (cl-charms:raw))
        (:noraw (cl-charms:noraw))
        (:cbreak (cl-charms:cbreak))
        (:nocbreak (cl-charms:nocbreak))
        (:cursor (cl-charms:curs-set 1))
        (:nocursor (cl-charms:curs-set 0))
        (:delay (cl-charms:nodelay cl-charms:*stdscr* 0))
        (:nodelay (cl-charms:nodelay cl-charms:*stdscr* 1))
        (:keypad (cl-charms:keypad cl-charms:*stdscr* 1))
        (:nokeypad (cl-charms:keypad cl-charms:*stdscr* 0))
        (:meta (cl-charms:meta cl-charms:*stdscr* 1))
        (:nometa (cl-charms:meta cl-charms:*stdscr* 0))
        (:colors (init-color)))))
  (do-delayed-init)
  (cl-charms:clear)
  (cl-charms:keypad cl-charms:*stdscr* 1)
  (cl-charms:curs-set 0)
  (cl-charms:refresh)
  (resize)
  nil)

(defun destroy-screen ()
  "Return terminal to default mode"
  (unless *running*
    (error "Screen is not initialized"))
  (cl-charms:endwin)
  (setf *running* nil)
  nil)

(defmacro with-screen ((&body arguments) &body body)
  "Ensures that wrapped code will be executed after successful
initialization of screen and that screen will be properly
deinitialized after `body' is executed (or reaised error)."
  `(unwind-protect
        (progn (init-screen ,@arguments)
               ,@body)
     (destroy-screen)))

(defun init-color ()
  (cond ((= (cl-charms:has-colors) 1)
         (cl-charms:start-color)
         (setf *used-colors* (list 0))
         (setf *used-color-pairs* (list 0 1 2 3 4 5 6 7)))
        (t
         (error "Your terminal doesn't support color"))))

(defclass root-frame (canvas-frame container-frame) ())

;;; Root frame definition
(sunless (frame :root)
  (setf it
        (make-instance 'root-frame)))

(defvar *display* :root)

(defun display (&optional (frame :root))
  "Set the root frame. Only it and its children will be displayed.
  Default is :ROOT frame."
  (setf *display* frame)
  (resize)
  (refresh))
