(in-package #:cocktus)

(defparameter *the-window* nil)

(defun arise (&rest FUCK)
#+darwin(lispbuilder-sdl-cocoahelper::cocoahelper-init)
(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero) (erect)))

(defun erect ()
	(sdl:with-init ()
	    (emerge)
	    (sdl:with-events ()
		    (:quit-event () t)
        (:video-resize-event (:W W :H H)
          (sdl:resize-window W H)
          (window-resized))
		    (:idle ()
	           (think)
	           (sdl:update-display)))
	    (cocktus::quit)))

(defun window-resized ()
  (sdl-cffi::sdl-put-env "SDL_VIDEO_WINDOW_POS=80,80")
  (initialize-my-textures))

(defun make-window (window-width window-height)
  (sdl:window window-width window-height 
    :title-caption "a fuck made by terminal256" 
    :flags '(sdl:sdl-opengl sdl:sdl-resizable))

  (initialize-my-textures))

(defun emerge ()
  ;;Telling opengl where to look for the surface
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
 
  (make-window 512 512)
  (setf (sdl:frame-rate) 60)
  )

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
    ((is-key-down "I") (draw-regular-quadrangle))
    (t (draw-regular-triangle)))
  ;; finish the frame
  (gl:flush)
	)

(defun quit ()
  (gl:delete-textures (list *the-texture*))
  (sb-ext::quit))