
(defpackage cl-tui
  (:use :cl
        :alexandria
        :anaphora)
  (:export #:init-screen
           #:destroy-screen

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
