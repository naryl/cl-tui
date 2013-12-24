
(defpackage cl-tui
  (:use :cl
        :anaphora)
  (:export #:init-screen
           #:destroy-screen

           #:refresh
           #:retained-frame
           #:text-frame
           #:callback-frame

           #:putchar
           #:add-text
   ))
