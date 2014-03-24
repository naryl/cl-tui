
(defpackage cl-tui
  (:use :cl
        :let+
        :alexandria
        :anaphora)
  (:export #:init-screen
           #:destroy-screen

           ; Other stuff
           #:frame-size
           #:define-frame
           #:destroy-frame

           ; Updates
           #:display
           #:refresh

           ; Canvas frames
           #:retained-frame
           #:callback-frame
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
