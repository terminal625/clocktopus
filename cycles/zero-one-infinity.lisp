(in-package #:t625)

(defun arise (&rest arguments)

  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)

  ;;Turning off floating point errors for copatibility or some shit
  #+sbcl(sb-int:set-floating-point-modes :traps '()) 
  
  ;;This is the initialization macro
  (sdl:with-init () 


    ;;init files subject to changes
    (hatch)

    (sdl:enable-unicode)
    ;;Telling opengl where to look for the surface
     (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

    ;;Macro for handling the main loop and processing input
    (sdl:with-events ()

      ;(:ACTIVE-EVENT (:GAIN GAIN :STATE STATE)) <- code for active windows

      ;;we map the sdl ascii characters to lisp characters
      (:KEY-DOWN-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
        (keyboard::key-down key UNICODE))

      (:KEY-UP-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
        (keyboard::key-up key))

      ;;this has to be here or else the window will not close
      (:quit-event () 

        ;;custom quit procedure
        ;;has to return true for the window to exit
        (burn))

      ;;the procedure when no events are going on
      (:idle ()
                          ; --(print keyboard::pressed-keys)--
                          ;;(print 89)
        ;;main loop
       (fly)))))






















