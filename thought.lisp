(in-package #:cocktus)

(defun arise (&rest cocktopussy)
	(sdl:with-init ()
	    (emerge)
	    (sdl:with-events ()
		    (:quit-event () t)
		    (:idle ()
	           (think)
	           (sdl:update-display)))
	    (die)))

(defun emerge ()
  (sdl:window 512 512 :title-caption "a fuck made by terminal256" :flags '(sdl:sdl-opengl))
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
  (setf (sdl:frame-rate) 60)

  (setf *the-texture* (car (gl:gen-textures 1)))
  (make-my-texture *the-texture* magic-smile)
  )


(defun think ()
	"draw a frame"
  ;;(gl:clear :color-buffer-bit)
  ;; if our texture is loaded, activate it and turn on texturing
  (gl:enable :texture-2d)

	(when *the-texture*
	(gl:bind-texture :texture-2d *the-texture*))
  ;;draw the entire square white so it doens't interfere with the texture
  (gl:color 1 0 0)
  (draw-regular-quadrangle)
  (draw-regular-triangle)
  ;; finish the frame
  (gl:flush)
	)

(defun die ()
  (gl:delete-textures (list *the-texture*))
  )