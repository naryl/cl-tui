
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "macro")
                 (:file "frame-base")
                 (:file "frames")
                 (:file "drawers")
                 (:file "cl-tui")
                 (:file "input")
                 )
    :depends-on (cl-charms
                 let-plus
                 alexandria
                 anaphora
                 split-sequence
                 vedit
                 osicat))
