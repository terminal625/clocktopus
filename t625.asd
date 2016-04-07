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
         ((:file magic)
          (:file thought)))))))