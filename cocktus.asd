(asdf:defsystem #:cocktus
  :depends-on (#:cl-opengl
               #:lispbuilder-sdl
               #:cl-utilities)

  :components  
  	((:file "package")
  	(:file "magic")
  (:module	"nature"
    :components
		( (:file "birth")
		  (:file "life")
		  (:file "death")))

  (:module "man-to-machine"
	   :components 
     ( (:file "keyboard")
			 (:file "mouse")))
  (:module "machine-to-man"
	   :components 
     ((:file "audio")
			(:file "graphics")))))
