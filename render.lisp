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

(defun draw-regular-triangle ()
   ;; draw a triangle
  (gl:with-primitive :triangles
    (gl:color   1 0 0)
    (gl:vertex -1 (/ -1 (sqrt 3)) 0)
    (gl:color   0 1 0)
    (gl:vertex  0 (/ 2 (sqrt 3)) 0)
    (gl:color   0 0 1)
    (gl:vertex  1 (/ -1 (sqrt 3)) 0)))

(defun draw-regular-quadrangle ()
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
