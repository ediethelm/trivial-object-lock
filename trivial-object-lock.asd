;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-object-lock
  :name "trivial-object-lock"
  :description "A simple method to lock object (and slot) access."
  :long-description ""
  :version "0.3.2"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-utilities
	       :log4cl
	       :bordeaux-threads
	       :iterate
	       :mgl-pax
	       :cl-annot)
  :in-order-to ((test-op (test-op :trivial-object-lock/test)))
  :components ((:file "package")
	       (:file "trivial-object-lock")
	       (:file "documentation")))

(defsystem :trivial-object-lock/test
  :name "trivial-object-lock/test"
  :description "Unit Tests for the trivial-object-lock project."
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:trivial-object-lock fiveam)
  :perform (test-op (o s) (uiop:symbol-call :fiveam  '#:run! :trivial-object-lock-tests))
  :components ((:file "test-trivial-object-lock")))
