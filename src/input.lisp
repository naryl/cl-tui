
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

(defvar *non-blocking-window* nil)

(defun read-key ()
  "Returns three values indicating the pressed keys.

Primary is the pressed key as a CL character or a keyword if it's a special key.

Secondary value is the state of alt key. CL-TUI tries its best to portably guess when ESC
and when alt+smth keys were pressed but seems like ncurses still returns garbage for
alt+function (i.e. all non-printable) keys.

Tertiary value is the state of ctrl key. If it's T then primary should be a lowercase
ascii code.

Shift key is reported by making the primary value uppercase and only if ctrl was not held
otherwise it's ignored (blame legacy terminals).

Also note that RETURN is reported as ^j and TAB as ^i."
  (when *need-resize*
    (setf *need-resize* nil)
    (ncurses-resize))
  ;; Using SETF instead of LET because sigwinch handler doesn't see the dynamic binding
  (setf *in-getch* t)
  (let ((key (cl-charms:wget-wch cl-charms:*stdscr*))
        ctrl)
    (setf *in-getch* nil)
    (when (eq key :error)                 ; No input in non-blocking mode
      (return-from read-key (values nil nil nil)))
    (when (key-ctrl-p key)
      (setf ctrl t)
      (setf key (ctrl-key-key key)))
    (let ((char (cond ((equal (keyname key) "KEY_RESIZE") ; Ignore key_resize completely
                       (read-key))
                      ((key-esc-p key)
                       (if (detect-alt-sequence key)
                           (return-from read-key (read-key-with-alt))
                           :esc))
                      ((key-function-p key)
                       (key-keyword key))
                      (t                  ; Simple characters including unicode
                       (code-char key)))))
      (values char nil ctrl))))

(defun key-function-p (key)
  (<= 256 key 633))

(defun key-esc-p (key)
  (= key 27))

(defun key-ctrl-p (key)
  (and (<= 0 key 31)
       (not (= key 27))))

(defun ctrl-key-key (key)
  (char-code (char-downcase (elt (keyname key) 1))))

(defun detect-alt-sequence (key)
  "Returns T if it's an ALT-sequence for the next key and NIL if it's a standalone ESC"
  (let ((next-key (cl-charms:wget-wch *non-blocking-window*)))
    (case next-key
      (27 (cl-charms:unget-wch next-key)
          nil)
      (:error nil)
      (t (cl-charms:unget-wch next-key)
         t))))

(defun read-key-with-alt ()
  (let+ (((:values char _alt ctrl) (read-key)))
    (values char t ctrl)))

(defun keyname (key)
  (cffi:foreign-string-to-lisp (cl-charms:keyname key)))

(defun key-keyword (key)
  (make-keyword (map 'string (lambda (c)
                               (case c
                                 (#\_ #\-)
                                 (t c)))
                     (keyname key))))

(defun ncurses-resize ()
  (cl-charms:endwin)
  (cl-charms:refresh)
  (resize)
  (refresh))
