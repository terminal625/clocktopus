(in-package #:cocktus)

(defun easy-pathname (filename)
  (merge-pathnames (concatenate 'string filename ".lisp")) *load-truename*)

(defun expand-and-load (path)
  (load (easy-pathname path))
  )

(defun my-zero ()
  (window-my-enter)
	(render-my-enter))

(defun my-one ()
  (window-my-fucks)
  (render-my-fucks))

(defun my-infinity ()
  (window-my-leave)
  (render-my-leave))

(defun arise (args)
    (window-start #'my-zero #'my-one #'my-infinity))

