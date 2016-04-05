(in-package #:cocktus)

;;takes a the key's character or name tests the state
(defun is-key-down (the-key)
  (sdl:GET-KEY-STATE (read-from-string (concatenate 'string ":SDL-KEY-" the-key))))