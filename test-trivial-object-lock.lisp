(in-package :trivial-object-lock)

(5am:def-suite :trivial-object-lock-tests :description "Trivial Object Lock tests")
(5am:in-suite :trivial-object-lock-tests)

(5am:test acquire-release
  (let ((lock1 (gensym)))
    (5am:finishes (acquire-lock lock1 nil))
    (5am:finishes (release-lock lock1 nil))))

(5am:test with-object-lock-held/threaded
  (let ((result nil)
	(expected '((F E C D B A) (F E D C B A)))
	(object (gensym))
	(prop1 (gensym))
	(prop2 (gensym)))

    (acquire-lock object prop1)

    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property prop2) (sleep 1) (push 'A result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object) (sleep 1) (push 'B result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property prop1) (sleep 1) (push 'C result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property prop2) (sleep 1) (push 'D result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property prop2)  (sleep 1) (push 'E result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object) (sleep 1) (push 'F result))))

    (sleep 2)
    (release-lock object prop1)
    (sleep 8)

    (5am:is (member result expected :test #'equalp))))

(5am:test recursive-lock
  (let ((object (gensym))
	(prop1 (gensym))
	(res nil))

    (bordeaux-threads:make-thread #'(lambda ()
				      (acquire-lock object nil)
				      (with-object-lock-held
					  (object :property prop1)
					(setf res 42))
				      (release-lock object nil)))

    (sleep 2)
    (5am:is (eq res 42))))
