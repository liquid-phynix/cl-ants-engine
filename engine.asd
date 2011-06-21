; vim: set filetype=lisp autoindent:

(in-package :cl-user)

(asdf:defsystem :engine
                :serial t
                :components ((:file "package")
                             (:file "helpers")
                             (:file "gen-indices")
;                             (:file "dummy-player")
			     (:file "pathfinding-player")
                             (:file "board-class")
                             (:file "engine"))
                :depends-on (:vis))

