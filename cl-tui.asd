
(defsystem cl-tui
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "cl-tui"))
    :depends-on (cl-charms))
