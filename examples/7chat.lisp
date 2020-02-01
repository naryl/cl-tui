
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(define-frame log (log-frame) :on :root)
;; edit-frame implements a single-line text editor. It will misbehave if its height is not 1
(define-frame input (edit-frame :prompt "> ") :on :root :h 1)

(defun finish-input ()
  ;; Get text from edit-frame
  (let ((text (get-text 'input)))
    ;; Append it to the log-frame
    (append-line 'log text)
    ;; And clear the text in edit-frame
    (clear-text 'input)))

(defun start ()
  (with-screen ()
    (set-split-type :root :vertical)
    (append-line 'log "Enter some text.")
    (append-line 'log "Esc to quit")
    (loop
       (refresh)
       (let ((key (read-key)))
         (case key
           ;; Esc and Newline are handled here
           (#\Esc (return))
           (#\Newline (finish-input))
           (:key-up (cl-tui:scroll-log 'log 1))
           (:key-down (cl-tui:scroll-log 'log -1))
           ;; Everything else is sent to the edit-frame.
           (t (handle-key 'input key)))))))
