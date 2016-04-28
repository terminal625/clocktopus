(in-package #:sandbox)

(defun arise (&rest arguments)
  ;;Setting up cocoa for macosx integration
  #+darwin (lispbuilder-sdl-cocoahelper::cocoahelper-init)
  ;;Turning off floating point errors for copatibility or some shit
  #+sbcl(sb-int:set-floating-point-modes :traps '())
  ;;This is the initialization macro
  (sdl:with-init ()
    (sdl:enable-unicode)
    ;;Telling opengl where to look for the surface
    
    (hatch)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    ;;Macro for handling the main loop and processing input
    (sdl:with-events ()
      ;;we map the sdl ascii characters to lisp characters
      (:KEY-DOWN-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
		       (keyboard::key-down key UNICODE))
      (:KEY-UP-EVENT (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :UNICODE UNICODE)
		     (keyboard::key-up key))
      (:MOUSE-BUTTON-DOWN-EVENT (:BUTTON BUTTON :STATE STATE :X X :Y Y))
      (:quit-event () t)
      ;;the procedure when no events are going on
      (:idle () (inject-me)))))

(let ((16/9-aspect-ratios  '((320 180)   (480 270)   (640 360) 
			     (848 480)   (1280 720)  (1920 1080)))
      (4/3-aspect-ratios   '((640 480)   (800 600)   (960 720) 
			     (1024 768)  (1280 960)  (1400 1050) 
			     (1440 1080) (1600 1200) (1856 1392)
			     (1920 1440) (2048 1536)))
      (16/10-aspect-ratios '((1280 800)  (1440 900)  (1680 1050) 
			     (1920 1200) (2560 1600))))
  (defun hatch ()
    (flet ((set-aspect-ratio (x) 
	     (setf window-width (car x) window-height (cadr x))))
      (set-aspect-ratio (nth 0 4/3-aspect-ratios)))
    ;;Setting the window
    (sdl:window window-width window-height :opengl t
		:opengl-attributes '((:sdl-gl-depth-size   16)
				     (:sdl-gl-doublebuffer 1)))
    ;;60 Hertz framerate for our 60 Hertz monitors of course
    (setf (sdl:frame-rate) 60)
    
    (setf *the-texture* (car (gl:gen-textures 1)))
    (make-my-texture *the-texture* magic-smile)
    
    (init-gl)))

(defun init-gl ()
  (gl:viewport 0 0 window-width window-height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective FOV (/ window-width window-height) 1 1000.0)
  (gl:matrix-mode :modelview)
  (gl:cull-face :back)
  (gl:clear-color 0.5 0.5 0.5 0)
  (gl:polygon-mode :front :fill)
  (gl:draw-buffer :back)
  (gl:enable :cull-face :depth-test))

(defparameter FOV 100)
(defparameter *camera-pitch* 0)
(defparameter *camera-yaw* 0)

(defparameter *friction* #(0.8 0.8 0.8))
(defparameter *up-vector* #(0 1 0))
(defparameter *looking-at* #(0 0 -3))

(defparameter *the-texture* nil)

(defparameter *window-caption* nil)
(defparameter *window-mini-caption* nil)
(defparameter window-height 512)
(defparameter window-width 512)
(defparameter *ticks* 0)

(defparameter mouse-sensitivity (/ 1 180))
(defparameter *mouse-delta* nil)

(defparameter controls "fs ade")

(defmacro nope (&rest nope))

(defmacro call (function-table nombre &rest args)
  `(funcall (gethash (string (quote ,nombre)) ,function-table) ,args))

(defun particle (position velocity)
  (let ((self (make-hash-table))
	(pos position)
	(vel velocity))  
    (macrolet ((def (name args &body body)
		   `(setf (gethash (string (quote ,name)) self) (lambda ,args ,@body)) ))
      (def :set-pos (x) (setf pos x))
      (def :get-pos () pos)
      (def :get-vel () vel)
      (def :set-vel (x) (setf vel x) ))
    self))

(defun inject-me ()
  (incf *ticks*)
  (keyboard::update) 
  (if (keyboard::key-p #\Esc) (sdl:push-quit-event))
  
  (let* ((x (cos *camera-yaw*))
	 (y (sin *camera-pitch*))
	 (z  (sin *camera-yaw*))
	 (xz (vector x z))
	 (lone (hypot xz))
	 (rect (scale xz (/ (cos *camera-pitch*) lone))))
    (setf *looking-at*
	  (vector (aref rect 0) y (aref rect 1))
	  ))
  (setf *mouse-delta* (mouse:delta))
  (setf *camera-yaw* (+ *camera-yaw* (* mouse-sensitivity (aref *mouse-delta* 0))))
  (setf *camera-pitch* (+ *camera-pitch* (* mouse-sensitivity (aref *mouse-delta* 1))))
  (setq *window-caption* (coerce keyboard::down-keys 'string)
	*window-mini-caption* "mini fuck")
  (sdl:set-caption *window-caption* *window-mini-caption*)
  
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
	     )
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:matrix-mode :modelview)    
      (gl:with-pushed-matrix
	(r-color)
	(gl:translate -1.8 0 0)
	(gl:scale 80 80 80)
	(gl:rotate (* *ticks* (/ pi 180) 40) 0 1 0)
	(glut:solid-teapot 1.3))
      
      (gl:with-pushed-matrix
	(gl:translate 0 0 -5)
	(r-color)
	(draw-teapot)
	)
      (gl:enable :texture-2d)
      (gl:enable :blend)
      (gl:bind-texture :texture-2d *the-texture*)
      ;; finish the frame
      (gl:flush)
      (sdl:update-display)
      ))
  
  (nope
   (defparameter camera (particle #(0.2 0.2 0.2) #(1 1 10)))
   (apply #'glu:look-at (concatenate 'list
				    (funcall camera 'get-pos)
				    (map 'vector #'+
					 *looking-at*
					  (funcall camera 'get-pos))
				    *up-vector*))
   (scalevf (call camera :get-vel) *friction*)
   (addvf (call camera :get-pos) (call camera :get-vel))
   (setf vel-total #(0 0 0))
   (dotimes (n 6)
     (if
      (keyboard::key-p (aref controls n))
      (addvf vel-total (aref 3d-control-forces n))))
   (let* ((wowzer (complex (aref vel-total 0) (aref vel-total 2)))
	  (howzie (complex (cos *camera-yaw*) (sin *camera-yaw*)))
	  (wibbly (* howzie wowzer))
	  (hello  (vector
		   (realpart wibbly)
		   (aref vel-total 1)
		   (imagpart wibbly))))
     (addvf (call camera :get-vel) vel-total))))

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
  (gl:tex-image-2d :texture-2d 0 :rgba 8 8 0 :luminance :unsigned-byte the-tex-data))

;;this motherfucker is what watches you RIGHT NOW LOLOL!LOL!
(defparameter magic-smile
  #(#xff #x00 #x00 #x00 #x00 #x00 #x00 #xff
    #x00 #x00 #xff #xff #xff #xff #x00 #x00
    #x00 #xff #x00 #xff #xff #x00 #xff #x00
    #x00 #xff #xff #xff #xff #xff #xff #x00
    #x00 #xff #x00 #xff #xff #x00 #xff #x00
    #x00 #xff #x30 #x00 #x00 #x30 #xff #x00
    #x00 #x00 #xff #xf2 #xff #xff #x00 #x00
    #xff #x00 #x00 #x00 #x00 #x00 #x00 #xff))
