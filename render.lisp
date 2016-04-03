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

(defparameter opengl-primitives  
  '(:points :lines :line-strip :line-loop :polygon :quads 
    :quad-strip :triangles :triangle-strip :triangle-fan))

(defun primitive-generator (choice vertex-data)
    (lambda () (gl:with-primitive (nth choice opengl-primitives) (mapcar #'eval vertex-data))))

(defun a-nice-function (funk the-list)
  (loop for x in the-list
    collect (cons funk x)))

(defun gl-2d-wrap (x y)    ;Takes only two arguments. Makes things easy
  (gl:vertex x y 0))

(defun gl-tex-wrap (x y)   ;Makes textures map like the screen does for simplicity
  (gl:tex-coord (/ (- 1 x) 2) (/ (- 1 y) 2)))

(defun interleave (&rest args)
  (apply #'mapcar #'list args))

(defun flatten (sequence) (loop for x in sequence append x))

(defun graph (point-sqeuence) 
  (let ((l (list-length point-sqeuence)) (i 0))
    (gl:with-primitive :line-strip
      (dotimes (i l t)
        (gl:vertex (- (* 2 (/ i l)) 1) (nth i point-sqeuence) 0))
        (incf i)
        )))

(defparameter a (a-nice-function 'gl-2d-wrap regular-triangle))
(defparameter b (a-nice-function 'gl:color rgb))
(defparameter a-plus-b (flatten (interleave b a)))

(defparameter c (a-nice-function 'gl-tex-wrap regular-square))
(defparameter d (a-nice-function 'gl-2d-wrap regular-square))

(defparameter draw-test-triangle (primitive-generator 7 a-plus-b))

(defparameter regular-quadrangle-vertex-data `( (gl:color   1 0.5 0)
                                                (gl-tex-wrap   -1  -1)
                                                (gl-2d-wrap    -1  -1)

                                                (gl:color   0.5 1 0)
                                                (gl-tex-wrap    1  -1)
                                                (gl-2d-wrap     1  -1)

                                                (gl:color   0 0.5 1)
                                                (gl-tex-wrap    1   1)
                                                (gl-2d-wrap     1   1)

                                                (gl:color   1 0 0.5)
                                                (gl-tex-wrap   -1   1)
                                                (gl-2d-wrap    -1   1)))

(defparameter regular-triangle-vertex-data `( (gl:color   1 1 0)
                                              (gl-2d-wrap   -1 ,(/ -1 (sqrt 3)))  
                                              (gl:color   0 1 1)
                                              (gl-2d-wrap  0 ,(/ 2 (sqrt 3)))
                                              (gl:color   1 0 1)
                                              (gl-2d-wrap   1 ,(/ -1 (sqrt 3)))))
                                               

(defparameter draw-regular-quadrangle 
  (primitive-generator 5  regular-quadrangle-vertex-data))


(defparameter draw-regular-triangle 
 (primitive-generator 7 regular-triangle-vertex-data ))