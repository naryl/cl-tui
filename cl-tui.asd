
(defsystem cl-tui
    :author "Alexander Sukhoverkhov"
    :description "High-level library for making Text User Interfaces"
    :license "MIT"
    :pathname "src/"
    :serial t
    :components ((:file "packages")
                 (:file "vedit")
                 (:file "macro")
                 (:file "frame-base")
                 (:file "frames")
                 (:file "layouts")
                 (:file "drawers")
                 (:file "cl-tui")
                 (:file "input")
                 )
    :depends-on (cl-charms
                 trivial-types
                 alexandria
                 anaphora
                 split-sequence
                 cl-containers
                 osicat))
