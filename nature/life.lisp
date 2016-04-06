(in-package #:t625)

(define-modify-macro unionf (&rest args) union)
(define-modify-macro set-differencef (&rest args) set-difference)

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
        (let ((character (list (code-char UNICODE))))
          (unionf (gethash KEY keyboard::sdl-ascii) character)
          (unionf keyboard::pressed-keys character)))

      (:KEY-UP-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
        (let ((dem-keys (gethash KEY keyboard::sdl-ascii)))
          (set-differencef keyboard::pressed-keys dem-keys)))


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
          (print keyboard::pressed-keys)
        ;;main loop
       (fly)))))    






















