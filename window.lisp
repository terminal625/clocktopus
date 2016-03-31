(in-package #:cocktus)

;;takes a the key's character or name tests the state
(defun keydown (the-key)
	(sdl:GET-KEY-STATE (read-from-string (concatenate 'string ":SDL-KEY-" the-key))))

(defun window-start (zero one infinity)
  	(sdl:with-init ()
	    (funcall zero)
	    (sdl:with-events ()
		    (:quit-event () t)
		    (:idle ()
	           (funcall one)
	           (sdl:update-display)))
	    (funcall infinity)))

(defun window-my-enter ()
	(sdl:window 720 720 :title-caption "a fuck made by terminal256" :flags '(sdl:sdl-opengl sdl:sdl-resizable))
  	;;creating the window 

  	(setf (sdl:frame-rate) 60)
  	;;setting the frame rate

  	;;(sdl-cffi::sdl-wm-grab-input sdl-cffi::sdl-enable)
  	;;grabbing the mouse

  	;;(sdl-cffi::sdl-show-cursor sdl-cffi::sdl-disable)
  	;;hiding the mouse

  	(sdl:disable-key-repeat)
  	;;disable keyrepeat

  	;; cl-opengl needs platform specific support to be able to load GL
  	;; extensions, so we need to tell it how to do so in lispbuilder-sdl
  	(setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address))

(defun window-my-fucks ())

(defun window-my-leave ()
	)
