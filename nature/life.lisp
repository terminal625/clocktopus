(in-package #:t625)

(defun evomer (a b)
  (remove b a))

(define-modify-macro evomerf (&rest args) evomer)

(defun arise (&rest arguments)

  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)

  ;;Turning off floating point errors for copatibility or some shit
  (sb-int:set-floating-point-modes :traps '()) 
  
  ;;This is the initialization macro
  (sdl:with-init () 


    ;;init files subject to changes
    (hatch)

    (sdl:enable-unicode)
    ;;Telling opengl where to look for the surface
     (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

    ;;Macro for handling the main loop and processing input
    (sdl:with-events ()

      (:ACTIVE-EVENT (:GAIN GAIN :STATE STATE))

      ;;we map the sdl ascii characters to lisp characters
      (:KEY-DOWN-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
        (setf (gethash KEY man-machine::sdl-ascii) (code-char UNICODE))
        (push (code-char UNICODE) man-machine::pressed-keys)

        (print man-machine::pressed-keys )
        )

      (:KEY-UP-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
        (evomerf man-machine::pressed-keys (gethash KEY man-machine::sdl-ascii)))


      (:MOUSE-MOTION-EVENT (:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL))
      (:MOUSE-BUTTON-DOWN-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y))
      (:MOUSE-BUTTON-UP-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y))

      ;;this has to be here or else the window will not close
      (:quit-event () 

        ;;custom quit procedure
        ;;has to return true for the window to exit
        (burn))

      ;;the procedure when no events are going on
      (:idle ()

        ;;main loop
       (fly)))))    






















