
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
  "Returns three values indicating the pressed keys.

Primary is the pressed key as a CL character or a keyword if it's a special key.

Secondary is the state of alt key if INIT-SCREEN was called with :META and your terminal
supports it. Among popular ones xterm does and rxvt doesn't. Otherwise alt will be
reported as a preceding :ESC keypress

Tertiary value is the state of ctrl key. If it's T then primary should be a lowercase
ascii among 32-63 codes.

Shift key is reported by making the primary value uppercase and only if ctrl was not held
otherwise it's ignored (blame legacy terminals)"
  (when *need-resize*
    (setf *need-resize* nil)
    (ncurses-resize))
  ;; Using SETF instead of LET because sigwinch handler doesn't see the dynamic binding
  (setf *in-getch* t)
  (let ((key (cl-charms:get-wch))
        alt ctrl)
    (setf *in-getch* nil)
    (when (<= 128 key 255) ; Detect alt with meta(TRUE)
      (setf alt t)
      (incf key -128))
    (when (and (<= 0 key 31) ; Detect ctrl
               (not (= key 27)))
      (setf ctrl t)
      (setf key (char-code
                 (char-downcase
                  (elt (keyname key) 1)))))
    (let ((char (cond ((equal (keyname key)
                              "KEY_RESIZE") ; Ignore key_resize completely
                       (read-key))
                      ((eql key 27)
                       :ESC)
                      ((<= 256 key 633) ; ncurses special key constants
                       (make-keyword (keyname key)))
                      (t (code-char key))))) ; Simple character
      (values char alt ctrl))))

(defun keyname (key)
  (cffi:foreign-string-to-lisp (cl-charms:keyname key)))

(defun ncurses-resize ()
  (cl-charms:endwin)
  (cl-charms:refresh)
  (resize)
  (refresh))
