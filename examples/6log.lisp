
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

;; Log frame displays lines of text. It has optional deduplication
;; (enabled in this example)
;; Attributes can be applied using custom line renderer (in a following example)
(define-frame log (log-frame :deduplicate-lines t) :on :root)

(defun start ()
  (with-screen (:colors)
    (append-line 'log "Press C-q to quit")
    (append-line 'log "A very long line: ~{~A~^ ~}" (make-list 40 :initial-element "asdf"))
    (append-line 'log "A very long word: ~A" (make-array 200 :initial-element #\x :element-type 'base-char))
    (loop
       (refresh)
       (let ((key (read-key)))
         (case key
           (#\Dc1 (return))
           (:key-up (cl-tui:scroll-log 'log 1))
           (:key-down (cl-tui:scroll-log 'log -1))
           (t (append-line 'log "~A" key)))))))
