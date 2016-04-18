(asdf:defsystem #:sandbox
  :description "when parts come together"
  :version "0.0.0"
  :author "a little boy in the sand"
  :maintainer "tain mainer"
  :licence "fuck that shit"

  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut
               #:lispbuilder-sdl
               #:cl-utilities)

  :serial t
  :components  
  ((:file "package")  

   (:module "man-machine"
      :components 
      ((:file "basic-input")))

   (:module "machine-man"
      :components 
      ((:file "audio")
       (:file "graphics")))

   (:module "cycles"
      :depends-on ("man-machine" "machine-man")
    	:components      
    	((:file "zero-one-infinity")
        (:module "ego"
         :components 
         ((:file thought)))))))