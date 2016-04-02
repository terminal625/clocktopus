(in-package #:cocktus)
;; a fucking dark arts world for creatures of the abyss


;;this motherfucker is what watches you RIGHT NOW LOLOL!LOL!
(defparameter magic-smile #( #xff #x00 #x00 #x00 #x00 #x00 #x00 #xff
							#x00 #x00 #xff #xff #xff #xff #x00 #x00
							#x00 #xff #x00 #xff #xff #x00 #xff #x00
							#x00 #xff #xff #xff #xff #xff #xff #x00
							#x00 #xff #x00 #xff #xff #x00 #xff #x00
							#x00 #xff #x30 #x00 #x00 #x30 #xff #x00
							#x00 #x00 #xff #xf2 #xff #xff #x00 #x00
							#xff #x00 #x00 #x00 #x00 #x00 #x00 #xff))

(defparameter rgb `((1 0 0)
                	(0 1 0)
                	(0 0 1)))

(defparameter regular-triangle `((-1 	,(/ -1 	(sqrt 3)))
                                 (0 	,(/ 	2 	(sqrt 3)))
                                 (1 	,(/ -1 	(sqrt 3)))))

(defparameter regular-square `((-1 -1)
                               (1 -1 )
                               (1  1 )
                               (-1  1)))

(defun one-random-zero ()
  (/ (random 1024) 1024))

;;In computer science, specifically formal languages, 
;;convolution (sometimes referred to as zip) 
;;is a function which maps a tuple of sequences into a sequence of tuples.

(defun zip (l1 l2)
	(mapcar #'list l1 l2))

(defun unzip (convlist)
  (apply #'mapcar #'list convlist))