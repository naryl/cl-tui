
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "macro")
                 (:file "frames")
                 (:file "layout")
                 (:file "drawers")
                 (:file "cl-tui")
                 (:file "input")
                 )
    :depends-on (cl-charms
                 let+
                 alexandria
                 anaphora))
