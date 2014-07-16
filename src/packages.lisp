
(defpackage cl-tui
  (:use :cl
        :let+
        :alexandria
        :anaphora
        :split-sequence)
  (:export #:init-screen
           #:destroy-screen
           #:with-screen

           ; Updates
           #:display
           #:refresh

           ; Frames
           #:frame
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

           ; Log frame
           #:log-frame
           #:append-line
           #:append-text
           #:clear

           ;; Tabbed frame
           #:tabbed-frame
           #:tabs-list
           #:tab-forward
           #:tab-backwards
           #:draw-tab-bar

           ; Input
           #:read-key
   ))
