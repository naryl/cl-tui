
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "frames")
                 (:file "cl-tui"))
    :depends-on (cl-charms))
