(in-package #:sandbox)

(defmacro progno (&rest nope))

(defun arise (&rest arguments)
  (declare (ignore arguments))
  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
  ;;Turning off floating point errors for copatibility or some shit
  #+sbcl(sb-int:set-floating-point-modes :traps '())
  (sdl:with-init ()
    (sdl:window
     128 128
     :opengl t
     :opengl-attributes '((:sdl-gl-depth-size 16)
			  (:sdl-gl-doublebuffer 1)))
    (win 128 128)
    (keyboard::initialize)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (macrolet
	((wowzer (&body events)
	   `(lambda ()	    
	      (loop until (= 0 (sdl-cffi::SDL-Poll-Event sdl-event)) do
		   (case (sdl::event-type sdl-event)
		     (:QUIT-EVENT (progn (setf quit t) (print "burned")))
		     ,@(mapcar #'(lambda (event)				     
				   (when (gethash (first event) sdl::*events*)
				     (sdl::expand-event 'sdl-event
							(car event)
							(gethash (car event) sdl::*events*)
							(cadr event)
							(cddr event))))
			       events)))
	      (unless quit
		(keyboard::update)
		(sdl::process-audio)))))
      (let* ((quit nil)
	     (sdl-event (sdl::new-event))
	     (base-function (wowzer 
			     ;;we map the sdl ascii characters to lisp characters
			     (:key-down-event
			      (:state state :scancode scancode :key key :mod mod :unicode unicode)
			      (progn
				(print (list state scancode key mod unicode))
				(print keyboard::down-keys))
			      (keyboard::key-down key unicode))   
			     (:key-up-event
			      (:state state :scancode scancode :key key :mod mod :unicode unicode)
			      (progn
				(print `(,state ,scancode ,key ,mod ,unicode))
				(print keyboard::down-keys))
			      (keyboard::key-up key))
			     (:mouse-button-down-event
			      (:button button :state state :x x :y y)
			      (progn (print `(,button ,state ,x ,y))))))
	     (inject-me (main)))
	(setf sdl::*sdl-event* sdl-event)
	(glut:init)
	(loop until quit do
	     (progn
	       (funcall base-function)
	       (funcall inject-me)))  
	(sdl::free-event sdl-event)))))

(defun make-window
    (&key
       ((:h height) 128)((:w width) 128)((:c caption) "fs ade")((:lil-cap little-caption) "hey ;]"))
  (let* ((h height) (w width) (change nil) (c caption) (lil-c little-caption)
	 (aspect-ratios '((16/9 . ((16 . 9) (20 30 40 53 80 120)))
			  (4/3 . ((4 . 3) (160 200 240 256 320 350 360 400 464 480 512)))
			  (16/10 . ((16 . 10) (80 90 105 120 160)))
			  (1/1 . ((1 1) (1 2 4 8 16 32 64 128 256 512 1024)))))
	 (self (make-hash-table)))
    (prog1
	(lambda (func &rest args) (apply (gethash func self) args))
      (setf (gethash :self self) (lambda () self))
      (mapcar (lambda (name-func)
		(setf (gethash (car name-func) self) (cadr name-func)))
	      (list
	       (list :get-height
		     (lambda () h))
	       (list :set-height
		     (lambda (x)
		       (unless (= h x)
			 (setf change t))
		       (setq h x)))       
	       (list :get-width
		     (lambda () w))
	       (list :set-width
		     (lambda (x)
		       (unless (= w x)
			 (setf change t))
		       (setq w x)))       
	       (list :get-caption
		     (lambda () c))   
	       (list :set-caption
		     (lambda (x) (setq c x)))
	       (list :get-mini-caption
		     (lambda () lil-c))   
	       (list :set-mini-caption
		     (lambda (x) (setq lil-c x)))
	       (list :push-dimensions
		     (lambda ()
		       (when change
			 (sdl:window
			  w h
			  :opengl t
			  :opengl-attributes '((:sdl-gl-depth-size 16)
					       (:sdl-gl-doublebuffer 1)))
			 (keyboard::initialize)
			 (setf change nil))))
	       (list :change
		     (lambda () (setf change t)))
	       (list :push-caption
		     (lambda ()
		       (sdl:set-caption c lil-c)))
	       (list :set-aspect-ratio
		     (lambda (ratio num)	 
		       (let* ((dat-list (assoc ratio aspect-ratios))
			      (vec-und-scales (cdr dat-list))
			      (x (caar vec-und-scales))
			      (y (cdar vec-und-scales))
			      (scale (nth num (cadr vec-und-scales)))
			      (up-and-down (* scale y))
			      (side-to-side (* scale x)))
			 (unless  (and (= w side-to-side) (= h up-and-down))
			   (setf change t))
			 (setf w side-to-side
			       h up-and-down)))))))))

(defun make-world (&key ((:p position) 0) ((:v velocity) 1))
  (let ((v velocity)
	(p position)
	(self (make-hash-table)))
    (prog1
	(lambda (func &rest args) (apply (gethash func self) args))
      (setf (gethash :self self) (lambda () self))
      (mapcar (lambda (name-func)
		(setf (gethash (car name-func) self) (cadr name-func)))
	      (list
	       (list :set-pos (lambda (x) (setf p x)))
	       (list :get-pos (lambda () p))       
	       (list :set-vel (lambda (x) (setf v x)))
	       (list :get-vel (lambda () v))   
	       (list :next (lambda () (setf p (+ p v))))
	       (list :prev (lambda () (setf p (- p v)))))))))

(defun make-particle
    (&key
       ((:p position))((:v velocity)) ((:o orientation)) ((:a angular-velocity)))
  (let ((pos position) (vel velocity) (self (make-hash-table)))
    (prog1
	(lambda (func &rest args) (apply (gethash func self) args))
      (setf (gethash :self self) (lambda () self))
      (mapcar (lambda (name-func)
		(setf (gethash (car name-func) self) (cadr name-func)))
	      (list
	       (list :get-pos (lambda () pos))
	       (list :set-pos (lambda (x) (setf pos x)))
	       (list :get-vel (lambda () vel))
	       (list :set-vel (lambda (x) (setf vel x))))))))

(defun main ()
  (let ((world (make-world :p 0 :v 1))
	(x 0)
	(y 0)
	(z 0)
	(window nil)
	(the-texture nil)
	( magic-smile
	 #(#xff #x00 #x50 #x60 100 #x00 #x00 #xff
	   #x00 #x00 #xff #x4f #xff #xff #x00 #x90
	   #x00 #x7f #x50 #x5f #xff #x00 #xff #x30
	   #x00 #xff #xff #xff #xff #xff #xff #xa0
	   #x00 #x7f #x45 #x7f #xff #x00 #xff #xe0
	   #x00 #xff #x30 #x70 #x00 #x30 #xff #xd0
	   #x00 #x70 #xff #x72 #xff #xff #x00 #x20
	   #xff #x00 #x50 #x70 #x00 #x00 #x00 #xff)))

    (lambda ()
      (macrolet ((cp (char then &optional else)
		   `(if (keyboard:key-p ,char)
			,then
			,else)))
	
	(funcall world :next)

	(unless window
	  (setf window (make-window)))
	(progn
	  (funcall window :set-aspect-ratio (/ 4 3) 0))
	(funcall window :push-dimensions)
	(cp #\\ (funcall window :change))

	(funcall window :set-caption
	   (coerce
	    (append keyboard:pressed-keys '(#\ )
		    keyboard:down-keys '(#\ )
		    keyboard:released-keys) 'string))
	(funcall window :push-caption)

	(gl:viewport 0 0 (funcall window :get-width) (funcall window :get-height))
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(glu:perspective 100 (/ (funcall window :get-width) (funcall window :get-height)) 1 1000.0)
	(gl:matrix-mode :modelview)
	(gl:cull-face :back)
	(gl:clear-color 0.5 0.5 0.5 0)
	(gl:polygon-mode :front :fill)
	(gl:draw-buffer :back)
	(gl:enable :cull-face :depth-test)
	
	
	(setf the-texture (car (gl:gen-textures 1)))
	(make-my-texture the-texture magic-smile)
	
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
		     (with-clockwise-winding (glut:solid-teapot 1.3)))
		   (make-my-texture (the-texture the-tex-data)
		     (gl:bind-texture :texture-2d the-texture)
		     (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
		     (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
		     (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
		     (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
		     (gl:tex-parameter :texture-2d :texture-border-color '(0 0 0 0))
		     (gl:tex-image-2d :texture-2d 0 :rgba 8 8 0 :luminance :unsigned-byte the-tex-data)))
	    (gl:clear :color-buffer-bit :depth-buffer-bit)
	    (gl:matrix-mode :modelview)
	    (gl:enable :texture-2d)
	    (gl:enable :blend)
	    (gl:blend-func :src-alpha :one-minus-src-alpha)
	    (gl:bind-texture :texture-2d the-texture)

	    (glu:look-at x y z 0 0 -5 0 1 0)
	    (gl:with-pushed-matrix
	      (gl:color 1 1 1)
	      (cp #\a (r-color))
	      
	      (gl:translate -1.8 0 0)
	      (gl:scale 80 80 80)
	      (gl:rotate (* (funcall world :get-pos) (/ pi 180) 40) 0 1 0)
	      (glut:solid-teapot 9))
	    
	    (gl:with-pushed-matrix
	      (gl:translate 1 1 -5)
	      (draw-teapot))
	    
	    (gl:with-pushed-matrix
	      (gl:color 1 1 1)
	      (if (keyboard:key-p #\a) (r-color))
	      (gl:translate 0 0 -10)
	      (gl:scale 8 2 1)
	      (gl:rotate (* (funcall world :get-pos) (/ pi 180) 40) 0 1 0)
	      (glut:solid-teapot 1.3))
	    
	    (gl:with-pushed-matrix
	      (gl:translate 0 0 -5)				
	      (draw-teapot))


	    (gl:disable :texture-2d))
	  (gl:flush)
	  (sdl:update-display))
	(cp #\Esc (progn
		    (sdl:push-quit-event)))))))

(defun scalevf (vec scalar)
  (map-into vec #'* scalar vec))
(defun addvf (vec delta)
  (map-into vec #'+ delta vec))
(defun hypot (vec)
  (sqrt (reduce (lambda (x y) (+ x (* y y))) vec :initial-value 0)))
(defun scale (vec scale)
  (map 'vector (lambda (x) (* scale x)) vec))
