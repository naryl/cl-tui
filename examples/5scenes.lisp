
(defpackage cl-tui.examples
  (:use :cl :cl-tui))

(in-package cl-tui.examples)

(defun render-scene (&key frame)
  (draw-box frame)
  (put-text frame 0 4 "~A" frame)
  (put-text frame 1 1 "Push 1, 2, 3, 4 to switch between frames ")
  (put-text frame 2 1 "Space to show all frames tiled")
  (put-text frame 3 1 "q to quit")
  (when (eq frame 'scene-4)
    (put-text frame 4 1 "This frame is not a part of :root frame")))

;; Three frames are on top of :root. By default if the :root frame is
;; displayed they'll be positioned horizontally.

;; define-children defines several frames placing them on the same other frame in order
;; Ensures that frames order won't be messed up if you redefine them individually
(define-children :root ()
  (scene-1 (simple-frame :render #'render-scene))
  (scene-2 (simple-frame :render #'render-scene))
  (scene-3 (simple-frame :render #'render-scene)))
;; Fourth frame is not positioned on any frame. It can only be displayed directly with a `display` call.
(define-frame scene-4 (simple-frame :render #'render-scene))

(defun start ()
  (read-line)
  (with-screen ()
    ;; `display` displays a frame and its children on the whole screen.
    ;; Before the first `display` call the :root frame is displayed
    (display 'scene-1)
    (loop (case (read-key)
            (#\q (return))
            (#\Space (display :root))
            (#\1 (display 'scene-1))
            (#\2 (display 'scene-2))
            (#\3 (display 'scene-3))
            (#\4 (display 'scene-4))))))
