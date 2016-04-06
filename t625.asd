(asdf:defsystem #:t625

  :description "when parts come together"
  :version "0.0.0"
  :author "terminal 256"
  :maintainer "terminal 625"
  :licence "fuck that shit"

  :depends-on (#:cl-opengl
               #:lispbuilder-sdl
               #:cl-utilities)

  :serial t
  :components  
  ((:file "package")
    (:file "magic")  

   (:module "man-machine"
      :components 
      ((:file "basic-input")))

   (:module "machine-man"
      :components 
      ((:file "audio")
       (:file "graphics")))

   (:module "nature"
      :depends-on ("man-machine" "machine-man")
    	:components      
    	((:file "life")
      (:file "thought")))))
