(asdf:defsystem #:cocktus
  :depends-on (#:cl-opengl
               #:lispbuilder-sdl
               #:cl-utilities)
  :serial t
  :components  ((:file "package")
                (:file "magic" )
                (:file "window")
                (:file "render")
                (:file "audio")
                (:file "thought")))