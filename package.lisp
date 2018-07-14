(mgl-pax:define-package #:trivial-object-lock
  (:documentation "trivial-object-lock provides a simple and probably naïve way to lock objects. Only real USP is its ability to lock object slots independently.")
  (:use #:common-lisp #:mgl-pax)
  (:export #:interrupt-execution
	   #:lock-timeout
	   #:acquire-lock
	   #:release-lock
	   #:with-object-lock-held))

