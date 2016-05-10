(defpackage #:sandbox
 	(:use #:cl)
 	(:export #:ARISE))

(defpackage #:man-machine
	(:nicknames keyboard mouse)
  	(:use #:cl)
  	(:export 
  		#:KEY-P
  		#:x
  		#:y
  		#:delta
  		#:left-p
  		#:middle-p
  		#:right-p
  		#:wheel-up
  		#:wheel-down
		#:key-released-p
		#:key-pressed-p
		#:down-keys
		#:pressed-keys
		#:released-keys))

(defpackage #:machine-man
	(:nicknames screen speaker)
 	(:use #:cl))

(in-package #:sandbox)

(defmacro progno (&rest nope))

(defmacro generate-case-events (event-name status &body events)
  `(loop until (= 0 (sdl-cffi::SDL-Poll-Event ,event-name)) do
	(case (sdl::event-type ,event-name)
	  (:QUIT-EVENT (progn (setf ,status t)))
	  ,@(mapcar (lambda (event)				     
		      (sdl::expand-event event-name
					 (car event)
					 (gethash (car event) sdl::*events*)
					 (cadr event)
					 (cddr event)))
		    events)))) 

(defun gen-base
    (&key ((:state status) nil) ((:event-obj sdl-event) (sdl::new-event)))
   (list 
    (cons :event-state (lambda () status))
    (cons
     :base
     (lambda ()
       (generate-case-events sdl-event status
	 ;;we map the sdl ascii characters to lisp characters
	 (:key-down-event
	  (:state state :scancode scancode :key key :mod mod :unicode unicode)	
	  (keyboard::key-down (pairlis
	    '(:state :scancode :key :mod :unicode)
	    (list state scancode key mod unicode))))   
	 (:key-up-event
	  (:state state :scancode scancode :key key :mod mod :unicode unicode)
	  (keyboard::key-up
	   (pairlis
	    '(:state :scancode :key :mod :unicode)
	    (list state scancode key mod unicode))))
	 (:mouse-button-down-event
	  (:button button :state state :x x :y y)))))
    (cons :wrapper (lambda (func)				
		     #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
		     ;;cocoa for macosx
		     ;;Turning off floating point errors for copatibility or some shit
		     #+sbcl(sb-int:set-floating-point-modes :traps '())
		     (sdl:with-init ()						    
		       (keyboard::initialize)
		       (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
		       (glut:init)
		       (setf sdl::*sdl-event* sdl-event)
		       (unwind-protect (funcall func))
		       (sdl:free-event sdl-event))))))

(defun arise
    (&rest args)
  (declare (ignore args))
  (let* ((basis (gen-base))
	 (quit-func (cdr (assoc :event-state basis)))
	 (base-func (cdr (assoc :base basis)))
	 (wrapper-func (cdr (assoc :wrapper basis))))
    (let* ((main (injection)))
      (sb-thread:make-thread (lambda ()
			       (funcall wrapper-func (lambda ()
						       (loop until (funcall quit-func) do
							    (keyboard::update)
							    (sdl::process-audio)
							    (funcall base-func)
							    (funcall main))
						       (print "burned"))))))))

(defun make-window
    (&key
       ((:storage storage) (make-hash-table))
       ((:self self) (lambda (func &rest args) (apply (gethash func storage) args)))
				     ((:height h) 128)
				     ((:width w) 128)
				     ((:caption c) "fs ade")
				     ((:little-caption lil-c) "hey ;]")
				     ((:change change) nil)
				     ((:aspect-ratios aspect-ratios)
				      '((16/9 . ((16 . 9) (20 30 40 53 80 120)))
					(4/3 . ((4 . 3) (160 200 240 256 320 350 360 400 464 480 512)))
					(16/10 . ((16 . 10) (80 90 105 120 160)))
					(1/1 . ((1 1) (1 2 4 8 16 32 64 128 256 512 1024))))))
  (prog1 self
    (mapcar (lambda (name-func)
	      (setf (gethash (car name-func) storage) (cadr name-func)))
	    (list
	     (list :get-height (lambda () h))
	     (list :set-height (lambda (x)
				 (unless (= h x)
				   (setf change t))
				 (setq h x)))       
	     (list :get-width (lambda () w))
	     (list :set-width (lambda (x)
				(unless (= w x)
				  (setf change t))
				(setq w x)))       
	     (list :get-caption (lambda () c))   
	     (list :set-caption (lambda (x) (setq c x)))
	     (list :get-mini-caption (lambda () lil-c))   
	     (list :set-mini-caption (lambda (x) (setq lil-c x)))
	     (list :push-dimensions
		   (lambda () (when change
				(sdl:window w h
					    :opengl t
					    :opengl-attributes
					    '((:sdl-gl-depth-size 16)
					      (:sdl-gl-doublebuffer 1)))
				(keyboard::initialize)
				(setf change nil))))
	     (list :change (lambda () (setf change t)))
	     (list :push-caption (lambda () (sdl:set-caption c lil-c)))
	     (list :set-aspect-ratio (lambda (ratio num)	 
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
					       h up-and-down))))))))

(defun make-world
    (&key
       ((:storage storage) (make-hash-table))
       ((:self self) (lambda (func &rest args) (apply (gethash func storage) args)))
				     ((:p p) 0)
				     ((:v v) 1))
  (prog1 self
    (mapcar (lambda (name-func)
	      (setf (gethash (car name-func) storage) (cadr name-func)))
	    (list
	     (list :set-pos (lambda (x) (setf p x)))
	     (list :get-pos (lambda () p))       
	     (list :set-vel (lambda (x) (setf v x)))
	     (list :get-vel (lambda () v))   
	     (list :next (lambda () (setf p (+ p v))))
	     (list :prev (lambda () (setf p (- p v))))))) )

(defun make-particle
    (&key
       ((:storage storage) (make-hash-table))
       ((:self self) (lambda (func &rest args) (apply (gethash func storage) args)))
				     ((:p pos))
				     ((:v vel))
				     ((:o orientation))
				     ((:a angular-velocity)))
  (prog1 self
    (mapcar (lambda (name-func)
	      (setf (gethash (car name-func) self) (cadr name-func)))
	    (list
	     (list :get-pos (lambda () pos))
	     (list :set-pos (lambda (x) (setf pos x)))
	     (list :get-vel (lambda () vel))
	     (list :set-vel (lambda (x) (setf vel x)))))))

(defun injection ()
  (macrolet ((cp (char then &optional else)
	       `(if (keyboard:key-p ,char)
		    ,then
		    ,else)))
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

	    (unless the-texture
	      (setf the-texture (car (gl:gen-textures 1)))
	      (make-my-texture the-texture magic-smile))
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

(in-package #:man-machine)

;; a file which handles the keyboard and mouse
;; mostly just wrappers around SDL


;; this is a hash table where the sdl key values
;; are the keys and the unicodes are the values
(defparameter sdl-ascii (make-hash-table))

;; this is a list of all the pressed keys,
;; in common lisp char format

(defun initialize ()
  (sdl:enable-unicode)
  (defparameter down-keys-prev nil)
  (defparameter down-keys nil)
  (defparameter pressed-keys nil)
  (defparameter released-keys nil))

;; getting the mouse x position
(defun x () (sdl:mouse-x))

;; getting the mouse y position
(defun y () (sdl:mouse-y))

;; returns a 2d vector corresponding to the change in 
;; mouse position
(defun delta () (sdl:mouse-relative-position))

;; functions which give the state of the mouse buttons
(defun right-p ()(sdl:mouse-right-p))
(defun left-p ()(sdl:mouse-left-p))
(defun middle-p ()(sdl:mouse-middle-p))

;; scrolling functions
(defun wheel-up ()(sdl:mouse-wheel-up-p))
(defun wheel-down () (sdl:mouse-wheel-down-p))

;; char to key conversion testing of shit 

(defun key-p (the-key) (member the-key down-keys))
(defun key-pressed-p (the-key) (member the-key pressed-keys))
(defun key-released-p (the-key) (member the-key released-keys))

;; TIL what a modify macro is
(define-modify-macro unionf (&rest args) union)
(define-modify-macro set-differencef (&rest args) set-difference)

(defun update ()
  (setf pressed-keys (set-difference down-keys down-keys-prev))
  (setf released-keys (set-difference down-keys-prev down-keys))
  (setf down-keys-prev down-keys))

;; helper function for the pressed key list
(defun key-down (info-list)
  (let ((character (list (code-char (cdr (assoc :unicode info-list))))))
    (unionf (gethash (cdr (assoc :key info-list)) sdl-ascii) character)
    (unionf down-keys character))
  (progn
    (print info-list)
    (print keyboard::down-keys)))

;; yet another helper function for the key list
(defun key-up (info-list) 
  (let ((dem-keys (gethash (assoc :key info-list) keyboard::sdl-ascii)))
    (set-differencef keyboard::down-keys dem-keys))
  (progn
    (print info-list)
    (print keyboard::down-keys)))

;; commenting out code. it is almost like junk dna
