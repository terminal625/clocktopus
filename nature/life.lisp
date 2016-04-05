(in-package #:cocktus)

(defun hatch ()
  
  (sdl:window 512 512 :flags '(sdl:sdl-opengl))

  (sdl:set-caption "a fuck made by terminal256" "mini fuck")

  (setf *the-texture* (car (gl:gen-textures 1)))

  (make-my-texture *the-texture* magic-smile) ;;setting up opengl after making the window

  (setf (sdl:frame-rate) 60))

(defparameter *the-texture* nil)
(defparameter args 3)

(defun fly ()
  ;"yolo baggins"
  ;"draw a frame"
  (gl:clear :color-buffer-bit)
  ;; if our texture is loaded, activate it and turn on texturing
  (gl:enable :texture-2d)

  (when *the-texture*
    (gl:bind-texture :texture-2d *the-texture*))

  ;(draw-regular-quadrangle)
  (if (is-key-down "M")  (draw-regular-triangle))

  (gl:color 0.5 0.2 0.9)
  (graph (g args))

  (if (is-key-down "ESCAPE") (burn))
  ;; finish the frame
  (gl:flush)
  (sdl:update-display)
  )                   