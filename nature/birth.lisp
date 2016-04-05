(in-package #:cocktus)

(defun arise (&rest FUCK)
  ;;Telling opengl where to look for the surface
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
  (sb-int:set-floating-point-modes :traps '()) 
  (erect))

(defun erect ()
  (sdl:with-init ()
    (hatch)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
       (fly)))
    (burn)))