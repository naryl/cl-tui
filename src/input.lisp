
(in-package cl-tui)

(defvar *in-getch* nil
  "Used to resize screen immediately instead of postponing it to the next input because
  characters put into queue by UNGETCH are not immediately read by GETCH. GETCH can
  receive them only on the next call.")

;;; FIXME:
;;; No idea what to do on e.g. Windows and a few other implementations without signal
;;; handlers support. Rely on ncurses build with --with-sigwinch for now.
#+sbcl
(sb-sys:enable-interrupt sb-posix:sigwinch
                         (lambda (a b c)
                           (declare (ignore a b c))
                           (if *in-getch*
                               (resize)
                               (cl-charms:ungetch cl-charms:key_resize))))

(defun read-key ()
  ;; Using SETF instead of LET because sigwinch handler doesn't see the dynamic binding
  (setf *in-getch* t)
  (let ((key (cl-charms:wgetch (slot-value (frame *display*) 'window))))
    (setf *in-getch* nil)
    (cond ((eql key cl-charms:key_resize)
           (resize)
           (read-key))
          (t key))))
