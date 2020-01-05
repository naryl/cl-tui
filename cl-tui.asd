
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "macro")
                 (:file "frame-base")
                 (:file "frames")
                 (:file "layouts")
                 (:file "drawers")
                 (:file "cl-tui")
                 (:file "input")
                 )
    :depends-on (cl-charms
                 let-plus
                 alexandria
                 anaphora
                 split-sequence
                 cl-containers
                 osicat))
