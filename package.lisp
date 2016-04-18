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
  		#:wheel-down))

(defpackage #:machine-man
	(:nicknames graphics audio)
 	(:use #:cl))