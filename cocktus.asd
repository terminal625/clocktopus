(asdf:defsystem #:cocktus
  :depends-on (#:cl-opengl
               #:cl-glu
               #:lispbuilder-sdl)
  :serial t
  :components  ((:file "package")
                (:file "magic" )
                (:file "cocktus")
                (:file "window")
                (:file "render")))