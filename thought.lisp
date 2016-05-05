(in-package #:sandbox)

(defun arise (&rest arguments)
  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
  ;;Turning off floating point errors for copatibility or some shit
  #+sbcl(sb-int:set-floating-point-modes :traps '())
  ;;This is the initialization macro
  (sdl:with-init ()  
    ;;Telling opengl where to look for the surface
    
    (setf window (make-window))
    (_ window :set-aspect-ratio (/ 4 3) 0)    
    ;;Setting the window
    (sdl:window
     (_ window :get-width) (_ window :get-height)
     :opengl t :opengl-attributes '((:sdl-gl-depth-size 16)(:sdl-gl-doublebuffer 1)))
    ;;60 Hertz framerate for our 60 Hertz monitors of course
    (setf (sdl:frame-rate) 60)  
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)

    (keyboard::initialize) 
    ;;macro for handling the main loop and processing input
    (sdl:with-events ()
      ;;we map the sdl ascii characters to lisp characters
      (:key-down-event
       (:state state :scancode scancode :key key :mod mod :unicode unicode)
       (keyboard::key-down key unicode))   
      (:key-up-event
       (:state state :scancode scancode :key key :mod mod :unicode unicode)
       (keyboard::key-up key))
      (:mouse-button-down-event
       (:button button :state state :x x :y y))
      (:quit-event () t)
      ;;the procedure when no events are going on
      (:idle ()
	     (keyboard::update)
	     (inject-me)	     
	     (sdl:update-display)))))

(defparameter window nil)

(defmacro nope (&rest nope))
(defun _ (funk &rest args) (apply funk args))

(macrolet 
    ((def (name args &body body)
       `(setf (gethash ,name function-table) (lambda ,args ,@body)))
     (make-class (&body body)
       `(let* ((function-table (make-hash-table))
	       (self (lambda (funky &rest args) 
		       (apply (gethash funky function-table) args)))) 
	  ,@body
	  self)))
  
  (defun make-particle (&key 
			  ((:p position)) 
			  ((:v velocity))
			  ((:o orientation))
			  ((:a angular-velocity)))
    (make-class
     (let ((pos position) 
	   (vel velocity))    
       (def :get-pos () pos)
       (def :set-pos (x) (setf pos x))
       (def :get-vel () vel)
       (def :set-vel (x) (setf vel x)))))
  
  (defun make-window (&key 
			((:h height) 512) 
			((:w width) 512)
			((:c caption) "fs ade")
			((:lil-cap little-caption) "hey ;]"))
    (make-class
     (let* ((h height)
	    (w width)
	    (c caption)
	    (lil-c little-caption)
	    (aspect-ratios
	     '((16/9 . ((16 . 9) (20 30 40 53 80 120)))
	       (4/3 . ((4 . 3) (160 200 240 256 320 350 360 400 464 480 512)))
	       (16/10 . ((16 . 10) (80 90 105 120 160)))
	       (1/1 . ((1 1) (1 2 4 8 16 32 64 128 256 512 1024))))))
    
       (def :get-height () h)
       (def :set-height (x) (setq h x))       
       (def :get-width () w)
       (def :set-width (x) (setq w x))       
       (def :get-caption () c)   
       (def :set-caption (x) (setq c x))
       (def :get-mini-caption () lil-c)   
       (def :set-mini-caption (x) (setq lil-c x))
       (def :push-caption ()
	 (sdl:set-caption c lil-c))
       (def :set-aspect-ratio (ratio num)
	 
	 (let* ((dat-list (assoc ratio aspect-ratios))
		(vec-und-scales (cdr dat-list))
		(x (caar vec-und-scales))
		(y (cdar vec-und-scales))
		(scale (nth num (cadr vec-und-scales))))
	   (setf w (* scale x)
		 h (* scale y)))))))
  
  (defun make-world (&key ((:p position) 0) ((:v velocity) 1))
    (make-class
     (let ((v velocity)
	   (p position))
       (def :set-pos (x) (setf p x))
       (def :get-pos () p)       
       (def :set-vel (x) (setf v x))
       (def :get-vel () v)   
       (def :next () (setf p (+ p v)))
       (def :prev () (setf p (- p v)))))))

(defmacro bind (char &rest body)
  `(setf (gethash ,char key-funcs) (lambda () ,@body)))

(let ((world (make-world :p 0 :v 1))
      (key-funcs (make-hash-table))
      (x 0)
      (y 0)
      (z 0))

  (defun inject-me ()

    (bind #\Esc () (sdl:push-quit-event))
    
    (dolist (key keyboard:down-keys)
      (funcall (gethash key key-funcs)))
    
    (_ world :next)
    
    (if (keyboard::key-p #\Esc) (sdl:push-quit-event))

    (_ window :set-caption
       (coerce
	(append keyboard:pressed-keys '(#\ )
		keyboard:down-keys '(#\ )
		keyboard:released-keys) 'string))
    (_ window :push-caption)

    (gl:viewport 0 0 (_ window :get-width) (_ window :get-height))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 100 (/ (_ window :get-width) (_ window :get-height)) 1 1000.0)
    (gl:matrix-mode :modelview)
    (gl:cull-face :back)
    (gl:clear-color 0.5 0.5 0.5 0)
    (gl:polygon-mode :front :fill)
    (gl:draw-buffer :back)
    (gl:enable :cull-face :depth-test)
    
    
    (setf *the-texture* (car (gl:gen-textures 1)))
    (make-my-texture *the-texture* magic-smile)
    
    (macrolet 
	((with-clockwise-winding (&body body)
	   `(progn 
	      (gl:front-face :cw) 
	      (progn ,@body)  
	      (gl:front-face :ccw))))
      (labels ((random-small () (/ (random 1024) 1024))
	       (r-color ()
		 (gl:color (random-small) (random-small) (random-small)))
	       (draw-teapot () 
		 (with-clockwise-winding (glut:solid-teapot 1.3))))
	(gl:clear :color-buffer-bit :depth-buffer-bit)
	(gl:matrix-mode :projection)
	(glu:look-at x y z 0 0.1 -5 0 1 0)
	(gl:matrix-mode :modelview)
	(gl:enable :texture-2d)
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(gl:bind-texture :texture-2d *the-texture*)

	
	(gl:with-pushed-matrix
	  (gl:color 1 1 1)
	  (if (keyboard:key-p #\a) (r-color))
	  (gl:translate -1.8 0 0)
	  (gl:scale 80 80 80)
	  (gl:rotate (* (_ world :get-pos) (/ pi 180) 40) 0 1 0)
	  (glut:solid-teapot 1.3))
	
	(gl:with-pushed-matrix
	  (gl:translate 0 0 -5)
					;(r-color)
	  (draw-teapot))
	
	(gl:flush)))))

(defun scalevf (vec scalar) (map-into vec #'* scalar vec))
(defun addvf (vec delta) (map-into vec #'+ delta vec))
(defun hypot (vec) (sqrt (reduce (lambda (x y) (+ x (* y y))) vec :initial-value 0)))
(defun scale (vec scale) (map 'vector (lambda (x) (* scale x)) vec))

(defun make-my-texture (the-texture the-tex-data)
  (gl:bind-texture :texture-2d the-texture)
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
  (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
  (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
  (gl:tex-image-2d :texture-2d 0 :rgba 4 4 0 :rgba :unsigned-byte the-tex-data))

(defparameter magic-smile
  #(#xff #x00 #x00 #x00 100 #x00 #x00 #xff
    #x00 #x00 #xff #x4f #xff #xff #x00 #x00
    #x00 #x7f #x00 #x5f #xff #x00 #xff #x00
    #x00 #xff #xff #xff #xff #xff #xff #x00
    #x00 #x7f #x00 #x7f #xff #x00 #xff #x00
    #x00 #xff #x30 #x70 #x00 #x30 #xff #x00
    #x00 #x70 #xff #x72 #xff #xff #x00 #x00
    #xff #x00 #x00 #x00 #x00 #x00 #x00 #xff))
