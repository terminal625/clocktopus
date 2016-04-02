(in-package #:cocktus)

(defparameter *the-window* nil)
(defparameter *ARGUMENTS* nil)

(defun arise (&rest FUCK)
(setf *ARGUMENTS* FUCK)
#+darwin(lispbuilder-sdl-cocoahelper::cocoahelper-init)
(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero) (erect)))

(defun erect ()
	(sdl:with-init ()
	    (emerge)
	    (sdl:with-events ()
		    (:quit-event () t)
		    (:idle ()
	           (think)
	           (sdl:update-display)))
	    (cocktus::quit)))

(defun make-opengl ()
  (initialize-my-textures))

(defun destroy-opengl ()
  (gl:delete-textures (list *the-texture*)))


(defun emerge ()
  ;;Telling opengl where to look for the surface
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

  (sdl:window 512 512 :flags '(sdl:sdl-opengl))
  (sdl:set-caption "a fuck made by terminal256" "mini fuck")

  (make-opengl) ;;setting up opengl after making the window

  (setf (sdl:frame-rate) 60))

(defun initialize-my-textures ()
  (setf *the-texture* (car (gl:gen-textures 1)))
  (make-my-texture *the-texture* magic-smile)
  )


(defun think ()
	"draw a frame"
  (gl:clear :color-buffer-bit)
  ;; if our texture is loaded, activate it and turn on texturing
  (gl:enable :texture-2d)

	(when *the-texture*
	(gl:bind-texture :texture-2d *the-texture*))
  ;;draw the entire square white so it doens't interfere with the texture
  (gl:color 1 0 0)
  (cond 
    ((is-key-down "I")  (funcall draw-regular-quadrangle))
    ((is-key-down "M")  (funcall draw-test-triangle))
    (t                  (funcall draw-regular-triangle)))
  (if (is-key-down "ESCAPE") (sdl:push-quit-event))
  ;; finish the frame
  (gl:flush)
	)

(defun quit () 
  (sdl:push-quit-event)
  (destroy-opengl)
  ;(sb-ext::quit)
  )