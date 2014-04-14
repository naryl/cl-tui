
(defpackage cl-tui
  (:use :cl
        :let+
        :alexandria
        :anaphora)
  (:export #:init-screen
           #:destroy-screen
           #:with-screen

           ; Updates
           #:display
           #:refresh

           ; Frames
           #:frame-size
           #:define-frame
           #:destroy-frame

           ; Common drawing routines
           #:with-attributes
           #:color
           #:color-pair
           #:clear-colors

           ; Containers
           #:container-frame

           ; Canvas frames
           #:retained-frame
           #:callback-frame
           #:draw-box
           #:put-char
           #:put-text

           ; Text frame
           #:text-frame
           #:append-line
           #:append-text
           #:clear

           ; Input
           #:read-key
   ))
