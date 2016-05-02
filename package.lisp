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
		#:released-keys
		))

(defpackage #:machine-man
	(:nicknames screen speaker)
 	(:use #:cl))
