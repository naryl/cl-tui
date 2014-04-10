
(in-package cl-tui)

(defvar *in-getch* nil
  "Used to resize screen immediately instead of postponing it to the next input because
  characters put into queue by UNGETCH are not immediately read by GETCH. GETCH can
  receive them only on the next call.")

(defvar *need-resize* nil)

;;; FIXME:
;;; No idea what to do on e.g. Windows and a few other implementations without signal
;;; handlers support.
#+sbcl
(defun sigwinch-handler (a b c)
  (declare (ignore a b c))
  (if *in-getch*
      (ncurses-resize)
      (setf *need-resize* t)))

#+sbcl
(sb-sys:enable-interrupt sb-posix:sigwinch #'sigwinch-handler)

(defun read-key ()
  ;; Using SETF instead of LET because sigwinch handler doesn't see the dynamic binding
  (when *need-resize*
    (setf *need-resize* nil)
    (ncurses-resize))
  (setf *in-getch* t)
  (let ((key (cl-charms:get-wch)))
    (setf *in-getch* nil)
    (cond ((eql key cl-charms:key_resize) ; Ignore it
           (read-key))
          ((<= 401 key 633) ; ncurses special key constants
           key)
          (t (code-char key))))) ; Simple character

(defun ncurses-resize ()
  (cl-charms:endwin)
  (cl-charms:refresh)
  (resize)
  (refresh))
