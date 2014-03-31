
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
        (:nocursor (cl-charms:curs-set 0)))))
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

;;; Root frame definition
(sunless (frame :root)
  (setf it
        (make-instance 'retained-frame
                       :name :root
                       :split-type :vertical)))

(defvar *display* :root)

(defun display (&optional (frame :root))
  "Set the root frame. Only it and its children will be displayed.
  Default is :ROOT frame."
  (setf *display* frame)
  (resize)
  (refresh))

(defmacro with-screen ((&body arguments) &body body)
  "Ensures that wrapped code will be executed after successful
initialization of screen and that screen will be properly
deinitialized after `body' is executed (or reaised error)."
  `(unwind-protect
        (progn (init-screen ,@arguments)
               ,@body)
     (destroy-screen)))

(defun get-attribute-name-from-keyword (attribute)
  "Converts keyword to ncurses attribute."
  (if (not (keywordp attribute))
    attribute
    (case attribute
      (:normal cl-charms:a_normal)
      (:standout cl-charms:a_standout)
      (:underline cl-charms:a_underline)
      (:reverse cl-charms:a_reverse)
      (:blink cl-charms:a_blink)
      (:dim cl-charms:a_dim)
      (:bold cl-charms:a_bold)
      (:protect cl-charms:a_protect)
      (:invis cl-charms:a_invis)
      (:altcharset cl-charms:a_altcharset))))

(defmacro with-attributes ((&body attributes) &body body)
  "Enables given attributes, executes body and then ensures
they're disabled."
  (let ((attribute-names (apply #'logior
                                (mapcar #'get-attribute-name-from-keyword attributes))))
    `(unwind-protect
          (progn
            (attron ,attribute-names)
            ,@body)
       (attroff ,attribute-names))))
