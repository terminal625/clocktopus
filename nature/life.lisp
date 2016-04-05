(in-package #:t625)

(defun arise (&rest arguments)

  ;;Telling opengl where to look for the surface
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)

  ;;Turning off floating point errors for copatibility or some shit
  (sb-int:set-floating-point-modes :traps '()) 
  
  ;;This is the initialization macro
  (sdl:with-init ()


    ;;init files subject to changes
    (hatch)

    ;;Macro for handling the main loop and processing input
    (sdl:with-events ()

      ;;this has to be here or else the window will not close
      (:quit-event () 

        ;;custom quit procedure
        ;;has to return true for the window to exit
        (burn))

      ;;the procedure when no events are going on
      (:idle ()

        ;;main loop
       (fly)))))

(defparameter *the-texture* nil)
(defparameter args 3)
(defparameter window-height 512)
(defparameter window-width 512)
(defparameter window-fps 60)

(defun hatch ()

  ;;Setting the window 
  (sdl:window window-width window-height :flags '(sdl:sdl-opengl ))

  ;;60 Hertz framerate for our 60 Hertz monitors of course
  (setf (sdl:frame-rate) window-fps)

  (setf *the-texture* (car (gl:gen-textures 1)))
  (make-my-texture *the-texture* magic-smile))

(defun fly ()
  (sdl:set-caption "a fuck made by terminal256" "mini fuck")

  (gl:clear :color-buffer-bit)
  (gl:enable :texture-2d)

  (gl:bind-texture :texture-2d *the-texture*)

  (draw-regular-quadrangle)
  (if (is-key-down "M")  (draw-regular-triangle))

  (gl:color 0.5 0.2 0.9)
  (graph (g args))
  ;; finish the frame
  (gl:flush)
  (sdl:update-display)

  (if (is-key-down "ESCAPE") (sdl:push-quit-event))
  )    

  (defun burn ()
    (let ((exit-fine t))
      (print "burned")
      exit-fine))               
























