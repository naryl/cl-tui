
(in-package cl-tui)

(defvar *running* nil "T when screen is initialized")

(defun init-screen (&rest arguments)
  "Initializes the ncurses environment"
  (when *running*
    (error "Screen is already initialized"))
  (setf *running* t)
  (sunless (osicat:environment-variable "ESCDELAY")
    (setf it "25"))
  (charms/ll:initscr)
  (sunless *non-blocking-window*
    (setf it (charms/ll:newwin 1 1 0 0))
    (charms/ll:wtimeout it 0))
  ;; There's no good way to handle resizing, especially on
  ;; Windows (PDCurses) without keypad and KEY_RESIZE
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (charms/ll:keypad *non-blocking-window* 1)
  (dolist (argument (list* :raw :noecho :nocursor arguments))
    (case argument
      (:echo     (charms/ll:echo))
      (:noecho   (charms/ll:noecho))
      (:raw      (charms/ll:raw))
      (:noraw    (charms/ll:noraw))
      (:cbreak   (charms/ll:cbreak))
      (:nocbreak (charms/ll:nocbreak))
      (:cursor   (charms/ll:curs-set 1))
      (:nocursor (charms/ll:curs-set 0))
      (:delay    (charms/ll:nodelay charms/ll:*stdscr* 0))
      (:nodelay  (charms/ll:nodelay charms/ll:*stdscr* 1))
      (:colors (init-color))))
  (do-delayed-init)
  (charms/ll:clear)
  (charms/ll:refresh)
  (resize)
  (values))

(defun destroy-screen ()
  "Return terminal to default mode"
  (unless *running*
    (error "Screen is not initialized"))
  (charms/ll:nocbreak)
  (charms/ll:keypad charms/ll:*stdscr* 0)
  (charms/ll:echo)
  (charms/ll:endwin)
  (setf *running* nil)
  (values))

(defmacro with-screen ((&body arguments) &body body)
  "Ensures that wrapped code will be executed after successful
initialization of screen and that screen will be properly
deinitialized after `body' is executed (or raised error)."
  `(unwind-protect
        (progn (init-screen ,@arguments)
               ,@body)
     (destroy-screen)))

(defun init-color ()
  (cond ((= (charms/ll:has-colors) 1)
         (charms/ll:start-color)
         (clear-colors))
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
