
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(define-frame log (log-frame) :on :root)
(define-frame input (edit-frame :prompt "> ") :on :root :h 1)

(defun finish-input ()
  (let ((text (get-text 'input)))
    (append-line 'log text)
    (clear-text 'input)))

(defun start ()
  (format t "Push Enter to start...~%")
  (read-line)
  (with-screen ()
    (set-split-type :root :vertical)
    (append-line 'log "Enter some text.")
    (loop
       (refresh)
       (let ((key (read-key)))
         (case key
           (#\Esc (return))
           (#\Newline (finish-input))
           (t (handle-key 'input key)))))))
