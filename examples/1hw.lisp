
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun start ()
  ;; with-screen evaluates its body with ncurses's initialized
  ;; screen. Almost nothing else (unless otherwise noted) works
  ;; outside of with-screen.
  (with-screen ()
    ;; Put some text on the :root frame which is created automatically
    (put-text :root 0 0 "Hello world!")
    ;; Commit drawing to terminal
    ;; Nothing on screen will be changed until this call
    (refresh)
    ;; Wait for a keypress (input will be described in a latter example)
    (read-key)))
