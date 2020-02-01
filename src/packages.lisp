
(defpackage :vedit
  (:use :common-lisp
        :containers)
  (:export #:make-vedit
           #:text #:point
           #:clear #:handle-key
           ))

(defpackage cl-tui
  (:use :cl
        :alexandria
        :anaphora
        :split-sequence)
  (:export #:init-screen
           #:destroy-screen
           #:with-screen

           ;; Updates
           #:display
           #:refresh

           ;; Frames
           #:frame-size
           #:define-frame
           #:define-children
           #:destroy-frame

           ;; Common drawing routines
           #:with-attributes
           #:color
           #:color-pair
           #:clear-colors

           ;; Containers
           #:container-frame
           #:set-split-type

           ;; Canvas frames
           #:simple-frame
           #:draw-box
           #:put-char
           #:put-text

           ;; Log frame
           #:log-frame
           #:append-line
           #:append-text
           #:clear
           #:scroll-log

           ;; Tabbed frame
           #:tabbed-frame
           #:tabs-list
           #:tab-forward
           #:tab-backwards
           #:draw-tab-bar

           ;; Edit frame
           #:edit-frame
           #:handle-key
           #:set-prompt
           #:get-text
           #:clear-text
           
           ;; Input
           #:read-key

   ))
