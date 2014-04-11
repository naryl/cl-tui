
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

Ctrl and Shift keys modify the primary value.
Ctrl+anything generates a non-printable character.
Shift key is reported by making the primary value uppercase. Ctrl+Shift can't be detected
at all."
  (when *need-resize*
    (setf *need-resize* nil)
    (ncurses-resize))
  ;; Using SETF instead of LET because sigwinch handler doesn't see the dynamic binding
  (setf *in-getch* t)
  (let ((key (cl-charms:wget-wch cl-charms:*stdscr*))
        (alt nil))
    (setf *in-getch* nil)
    (if (eq key :error) ; No input in non-blocking mode
        (values nil nil)
        (let ((char (cond ((equal (keyname key) "KEY_RESIZE") ; Ignore key_resize completely
                           (read-key))
                          ((key-esc-p key)
                           (cond ((detect-alt-sequence key)
                                  (setf alt t)
                                  (read-key))
                                 (t
                                  :esc)))
                          ((key-function-p key)
                           (key-keyword key))
                          (t                  ; Simple characters including unicode
                           (code-char key)))))
          (values char alt)))))

(defun key-function-p (key)
  (<= 256 key #.(parse-integer (format nil "~A" cl-charms:key_event)
                               :radix 8)))

(defun key-esc-p (key)
  (= key 27))

(defun detect-alt-sequence (key)
  "Returns T if it's an ALT-sequence for the next key and NIL if it's a standalone ESC"
  (let ((next-key (cl-charms:wget-wch *non-blocking-window*)))
    (case next-key
      (27 (cl-charms:unget-wch next-key)
          nil)
      (:error nil)
      (t (cl-charms:unget-wch next-key)
         t))))

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
