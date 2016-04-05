(defpackage #:t625
  (:use #:cl)
  (:export "ARISE"))

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
  ((:file "magic")

   (:module "man-to-machine"
      :components 
      ((:file "keyboard")
       (:file "mouse")))

   (:module "machine-to-man"
      :components 
      ((:file "audio")
       (:file "graphics")))

   (:module "nature"
      :depends-on ("man-to-machine" "machine-to-man")
    	:components      
    	((:file "life")))))
