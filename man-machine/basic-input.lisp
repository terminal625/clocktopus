(in-package #:man-machine)

;; a file which handles the keyboard and mouse
;; mostly just wrappers around SDL


;; this is a hash table where the sdl key values
;; are the keys and the unicodes are the values
(defparameter sdl-ascii (make-hash-table))

;; this is a list of all the pressed keys,
;; in common lisp char format 
(defparameter pressed-keys nil)

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
(defun key-p (the-key) (member the-key pressed-keys))


;; TIL what a modify macro is
(define-modify-macro unionf (&rest args) union)
(define-modify-macro set-differencef (&rest args) set-difference)


;; helper function for the pressed key list
(defun key-down (key unicode) 
	(let ((character (list (code-char UNICODE))))
          (unionf (gethash key keyboard::sdl-ascii) character)
          (unionf keyboard::pressed-keys character)))

;; yet another helper function for the key list
(defun key-up (key) 
	(let ((dem-keys (gethash key keyboard::sdl-ascii)))
          (set-differencef keyboard::pressed-keys dem-keys)))

;; commenting out code. it is almost like junk dna