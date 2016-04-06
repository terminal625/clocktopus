(in-package #:t625)

(defparameter *the-texture* nil)
(defparameter args 3)
(defparameter window-height 512)
(defparameter window-width 512)
(defparameter window-fps 60)

(defun hatch ()

  ;;Setting the window 
  (sdl:window window-width window-height :flags '(sdl:sdl-opengl) )

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
  (draw-regular-triangle)

  (gl:color 0.5 0.2 0.9)
  (graph (g args))
  ;; finish the frame
  (gl:flush)
  (sdl:update-display)

  ;;27 is the ascii escape key
  ;(if (man-to-machine:keyp #\Esc) (sdl:push-quit-event))
  )    

  (defun burn ()
    (let ((exit-fine t))
      (print "burned")
      exit-fine))           