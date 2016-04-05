(in-package #:cocktus)

(defun make-my-texture (the-texture the-tex-data)
  (gl:bind-texture :texture-2d the-texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
  (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
  (gl:tex-image-2d :texture-2d 0 :rgba 8 8 0 :luminance :unsigned-byte the-tex-data))

(defun graph (point-sqeuence) 
  (let ((l (list-length point-sqeuence)) (i 0))
    (gl:with-primitive :line-strip
      (dotimes (i l t)
        (gl:vertex (- (* 2 (/ i l)) 1) (nth i point-sqeuence) 0))
        (incf i)
        )))            


(defun gl-2d-wrap (x y)    ;Takes only two arguments. Makes things easy
  (gl:vertex x y 0))

(defun gl-tex-wrap (x y)   ;Makes textures map like the screen does for simplicity
  (gl:tex-coord (/ (- 1 x) 2) (/ (- 1 y) 2)))

(defun draw-regular-quadrangle ()
  (gl:with-primitive :quads
      (mapcar #'eval  regular-quadrangle-vertex-data)))


(defun draw-regular-triangle ()
 (gl:with-primitive :triangles
      (mapcar #'eval  regular-triangle-vertex-data)))

(defparameter regular-quadrangle-vertex-data (list
					      '(gl:color   1 0.5 0)
					      '(gl-tex-wrap   -1  -1)
					      '(gl-2d-wrap    -1  -1)

					      '(gl:color   0.5 1 0)
					      '(gl-tex-wrap    1  -1)
					      '(gl-2d-wrap     1  -1)

					      '(gl:color   0 0.5 1)
					      '(gl-tex-wrap    1   1)
					      '(gl-2d-wrap     1   1)

					      '(gl:color   1 0 0.5)
					      '(gl-tex-wrap   -1   1)
					      '(gl-2d-wrap    -1   1)))

(defparameter regular-triangle-vertex-data (list
					     '(gl:color   1 1 0)
					     `(gl-2d-wrap   -1 ,(/ -1 (sqrt 3)))  
					     '(gl:color   0 1 1)
					     `(gl-2d-wrap  0 ,(/ 2 (sqrt 3)))
					     '(gl:color   1 0 1)
					     `(gl-2d-wrap   1 ,(/ -1 (sqrt 3)))))