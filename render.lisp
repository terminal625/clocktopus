(in-package #:cocktus)

(defparameter *the-texture* nil)

(defun make-my-texture (the-texture the-tex-data)
	(gl:bind-texture :texture-2d the-texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
  (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
  (gl:tex-image-2d :texture-2d 0 :rgba 8 8 0 :luminance :unsigned-byte the-tex-data))

(defun one-random-zero ()
  (/ (random 1024) 1024))

(defun random-gl-color ()
  (gl:color (one-random-zero ) (one-random-zero ) (one-random-zero )))

(defun draw-triangle ()
   ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color   1 0 0)
    (gl:vertex -1 (/ -1 (sqrt 3)) 0)
    (gl:color   0 1 0)
    (gl:vertex  0 (/ 2 (sqrt 3)) 0)
    (gl:color   0 0 1)
    (gl:vertex  1 (/ -1 (sqrt 3)) 0)))

(defun draw-square ()
    ;; draw a square
  (gl:with-primitive :quads
    ;; texture coordinates per vertex
    (gl:tex-coord  0 10)
    (gl:vertex    -1 -1 0)
    (gl:tex-coord  10 10)
    (gl:vertex     1 -1 0)
    (gl:tex-coord  10 00)
    (gl:vertex     1  1 0)
    (gl:tex-coord  0 0)
    (gl:vertex    -1  1 0)))

(defun render-my-enter ()
  (setf *the-texture* (car (gl:gen-textures 1)))
  (make-my-texture *the-texture* data-smile)
  )

(defun render-my-fucks ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; if our texture is loaded, activate it and turn on texturing
  (gl:enable :texture-2d)

(when *the-texture*
(gl:bind-texture :texture-2d *the-texture*))
  ;;draw the entire square white so it doens't interfere with the texture
  ;(gl:color 1 1 1)
  (draw-square)
  (draw-triangle)
  ;; finish the frame
  (gl:flush))

(defun render-my-leave ()
  (gl:delete-textures (list *the-texture*))
  )
