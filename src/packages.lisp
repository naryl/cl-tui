
(defpackage cl-tui
  (:use :cl
        :anaphora)
  (:export #:init-screen
           #:destroy-screen

           #:display
           #:refresh
           #:retained-frame
           #:text-frame
           #:callback-frame

           #:put-char
           #:put-text
           #:add-text
           #:clear

           #:read-key
   ))
