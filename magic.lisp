(in-package #:cocktus)

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

(defparameter nums (list
  "01871c40 01871d70 01871e21 01871f01 01a71c30 01a71d01 01a71e13 01a71f90 01971c10 01971d31 01971e81 01971f01 01571c50 01571d41 01571e01 01571f01 01f71c20 01f71de1 01f71ec5 01f71f01 01e71c60 01e71de1 01e71e45 01e71f01 01471cf0 01471d00 01471e00 01471f40 01671cf0 01671d00 01671e00 01671f40 01771cf0 01771d00 01771e00 01771f40 01b71cf0 01b71d00 01b71e00 01b71f40 01c71cf0 01c71d00 01c71e00 01c71f40 01d71cf0 01d71d00 01d71e00 01d71f40"
  "00b71c40 00b71d70 00b71e21 00b71f01 00c71c30 00c71d01 00c71e13 00c71f90 00f71c10 00f71d30 00f71e81 00f71f01 00a71c50 00a71d40 00a71e01 00a71f01 01171c20 01171de0 01171ec5 01171f01 01071c60 01071de0 01071e45 01071f01 00d71c0f 00d71d00 00d71e00 00d71f40 00e71c0f 00e71d00 00e71e00 00e71f40 01571c0f 01571d00 01571e00 01571f40 01b71c0f 01b71d00 01b71e00 01b71f40"
  "00172000 00172100 0017226b 00172310 00971c50 00971d70 00971e21 00971f01 00a71c41 00a71d61 00a71e10 00a71f90 00b71c40 00b71d61 00b71e10 00b71f90 00c71c20 00c71d01 00c71e81 00c71f91 00d71c10 00d71d01 00d71ea0 00d71f90 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c30 00f71d01 00f71ec5 00f71f01 01071c60 01071d01 01071e45 01071f01 01271cf0 01271d00 01271e00 01271f40 01571cf0 01571d00 01571e00 01571f40"
  "00f71c40 00f71d00 00f71ec6 00f71fd0 00971c50 00971d00 00971ec0 00971fd0"
  "00f71c40 00f71d00 00f71ec6 00f71fd0 00971c50 00971d00 00971ec0 00971fd0"
  "00172000 00172100 00172200 00172300 01171c50 01171d40 01171e2b 01171f01 01271c40 01271d01 01271e10 01271f90 01371c41 01371d01 01371e10 01371f90 00d71c10 00d71d01 00d71ea0 00d71f90 00c71c20 00c71d30 00c71e8b 00c71f01 00f71c60 00f71de0 00f71e4b 00f71f01 01071c30 01071de0 01071ecb 01071f01 00e71cf0 00e71d00 00e71e00 00e71f40 01671cf0 01671d00 01671e00 01671f40"
  "00a71c50 00a71d40 00a71e2b 00a71f01 00b71c10 00b71d01 00b71ea0 00b71f90 00c71c40 00c71d01 00c71e10 00c71f90 00d71cf0 00d71d00 00d71e00 00d71f40 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c20 00f71d30 00f71e8b 00f71f01 01071c60 01071de0 01071e4b 01071f01 01171c30 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "00a71c50 00a71d40 00a71e2b 00a71f01 00b71c10 00b71d01 00b71ea0 00b71f90 00c71c40 00c71d01 00c71e10 00c71f90 00d71c41 00d71d01 00d71e10 00d71f90 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c20 00f71d30 00f71e8b 00f71f01 01071c60 01071de0 01071e4b 01071f01 01171c30 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "00f71c40 00f71d00 00f71ec6 00f71fd0 00971c50 00971d00 00971ec0 00971fd0"
  "00f71c40 00f71d00 00f71ec6 00f71fd0 00971c50 00971d00 00971ec0 00971fd0"
  "00f71c40 00f71d00 00f71ec6 00f71fd0 00971c50 00971d00 00971ec0 00971fd0"
  "00a71c40 00a71d40 00a71e2b 00a71f01 00b71cf0 00b71d00 00b71e00 00b71f40 00c71c30 00c71d01 00c71e10 00c71f90 00d71cf0 00d71d00 00d71e00 00d71f40 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c10 00f71d30 00f71e8b 00f71f01 01071c50 01071de0 01071e4b 01071f01 01171c20 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "00a71c50 00a71d40 00a71e2b 00a71f01 00b71c10 00b71d01 00b71ea0 00b71f90 00c71c40 00c71d01 00c71e10 00c71f90 00d71c41 00d71d01 00d71e10 00d71f90 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c20 00f71d30 00f71e8b 00f71f01 01071c60 01071de0 01071e4b 01071f01 01171c30 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "01971d30 01f71de1"
  "00a71c50 00a71d40 00a71e2b 00a71f01 00b71c10 00b71d01 00b71ea0 00b71f90 00c71c40 00c71d01 00c71e10 00c71f90 00d71cf0 00d71d00 00d71e00 00d71f40 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c20 00f71d30 00f71e8b 00f71f01 01071c60 01071de0 01071e4b 01071f01 01171c30 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "00a71c50 00a71d40 00a71e2b 00a71f01 00b71c10 00b71d01 00b71ea0 00b71f90 00c71c40 00c71d01 00c71e10 00c71f90 00d71cf0 00d71d00 00d71e00 00d71f40 00e71cf0 00e71d00 00e71e00 00e71f40 00f71c20 00f71d30 00f71e8b 00f71f01 01071c60 01071de0 01071e4b 01071f01 01171c30 01171de0 01171ecb 01171f01 01571cf0 01571d00 01571e00 01571f40 01b71cf0 01b71d00 01b71e00 01b71f40"
  "01471cf0 01471d00 01471e00 01471f40 01571c30 01571d40 01571e00 01571f01 01671cf0 01671d00 01671e00 01671f40 01771cf0 01771d00 01771e00 01771f40 01871c10 01871d40 01871e20 01871f01 01971c20 01971d30 01971e80 01971f01 01a71c50 01a71d01 01a71e10 01a71f90 01b71cf0 01b71d00 01b71e00 01b71f40 01c71cf0 01c71d00 01c71e00 01c71f40 01d71cf0 01d71d00 01d71e00 01d71f40 01e71c70 01e71de0 01e71e45 01e71f01 01f71c60 01f71de0 01f71ec5 01f71f01"
  ))

;;here is a list of opengl primitives
;; :points :lines :line-strip :line-loop :polygon :quads 
;; :quad-strip :triangles :triangle-strip :triangle-fan

(defun a-nice-function (funk the-list)
  (loop for x in the-list
     collect (cons funk x)))

(defun interleave (&rest args)
  (apply #'mapcar #'list args))

(defun flatten (sequence) (loop for x in sequence append x))         

;; a fucking dark arts world for creatures of the abyss

(defun one-random-zero ()
  (/ (random 1024) 1024))

;;In computer science, specifically formal languages, 
;;convolution (sometimes referred to as zip) 
;;is a function which maps a tuple of sequences into a sequence of tuples.

(defun zip (l1 l2)
  (mapcar #'list l1 l2))

(defun unzip (convlist)
  (apply #'mapcar #'list convlist))

(defun split (y)(cl-utilities:split-sequence #\Space y))
(defun fudge (n) (parse-integer n :junk-allowed t :radix 16))
(defun p-form (s) (float (- (/ (fudge s) (expt 2 24)) 1)))
(defun break-n-luate (a) (mapcar #'p-form (split a)))

(defun g (u) (break-n-luate (nth (mod u (list-length nums)) nums)))

