
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "frames")
                 (:file "drawers")
                 (:file "cl-tui"))
    :depends-on (cl-charms
                 anaphora))
