;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-object-lock)
(annot:enable-annot-syntax)

(defvar *locks-held* (make-hash-table :test #'equal))
(defvar *function-lock* (bordeaux-threads:make-lock "FunctionLock"))

(defvar *lock-queue-counter* 0)
(proclaim '(type fixnum *lock-queue-counter*))

(define-condition interrupt-execution (condition) ())
(define-condition lock-timeout (condition) ())

(defun acquire-lock (object property
		     &key (blocking t) (timeout nil) (test #'eq)
		     &aux (object-lock nil) (cond-var nil) (thread (bordeaux-threads:current-thread)))
  "Acquire a lock on the given *OBJECT*. When not *NIL*, *PROPERTY* will be used to create a *PARTIAL-LOCK* on *OBJECT*; otherwise a *FULL-LOCK* is created.

Returns *NIL* if the lock was acquired; otherwise it returns a conditional variable to wait on."
  (declare (type function test)
	   (type (or null integer) timeout)
	   (type boolean blocking))
  
  (bordeaux-threads:with-lock-held (*function-lock*)
    (multiple-value-bind (value found) (gethash object *locks-held*)
      (declare (type boolean found))
      (if found ;; (and found value)
	  (progn
	    (setf object-lock value)
	    (unless (or (getf object-lock :full) (getf object-lock :partial))
	      (log:error "The lock for object ~a was not removed by release-lock!" object)
	      (cerror "Ignore and continue." "The lock for object was not removed by release-lock!")
	      (return-from acquire-lock nil)))

	  ;; Create an object lock stub
	  (setf object-lock
		(setf (gethash object *locks-held*) (list :partial nil :full nil))))

      ;; If necessary mark to create a condition variable
      (when (or (and (getf object-lock :full)
		     (not (eq (third (first (getf object-lock :full))) thread)))
		(and (not property) (getf object-lock :partial))
		(and property (member property (getf object-lock :partial) :key #'first :test test)))
	(setf cond-var t))
      
      (if property
	  (let ((entry (cadar (member property (getf object-lock :partial) :key #'first :test test))))
	    (if entry
		(trivial-utilities:aif (member thread entry :key #'third)
				       (progn
					 (log:debug "Recursive lock detected.")
					 (incf (the fixnum (nth 4 (car it))))
					 (setf cond-var nil))
				       (progn
					 (log:debug "Creating new entry for property '~a'." property)
					 (nconc entry (list (list (get-universal-time)
								  (incf (the fixnum *lock-queue-counter*))
								  thread
								  (setf cond-var (if cond-var (bordeaux-threads:make-condition-variable) nil))
								  1)))))
		(setf (getf object-lock :partial)
		      (nconc (getf object-lock :partial) (list (list property
								     (list (list (get-universal-time)
										 (incf (the fixnum *lock-queue-counter*))
										 thread
										 (setf cond-var (if cond-var (bordeaux-threads:make-condition-variable) nil))
										 1))))))))
	  
	  (trivial-utilities:aif (member thread (getf object-lock :full) :key #'third)
				 (progn
				   (incf (the fixnum (nth 4 (car it))))
				   (setf cond-var nil))
				 (setf (getf object-lock :full) (nconc (getf object-lock :full)
								       (list (list (get-universal-time)
										   (incf (the fixnum *lock-queue-counter*))
										   thread
										   (setf cond-var (if cond-var (bordeaux-threads:make-condition-variable) nil))
										   1)))))))

    (when (and blocking cond-var)
      (when (bordeaux-threads:condition-wait cond-var *function-lock* :timeout timeout)
	(setf cond-var nil))))
  
  (return-from acquire-lock cond-var))

(defun find-next-waiting-lock (object &aux (next nil) (part nil))
  (multiple-value-bind (value found) (gethash object *locks-held*)
    (declare (type boolean found))
    (when (and found value)

      ;; The next full lock should be the second in row, tops.
      (assert (or (null (getf value :full))
		  (let ((pos (position-if #'identity (the list (getf value :full)) :key #'third)))
		    (if (or (null pos) (< (the fixnum pos) 2))
			t
			nil))))
      
      (setf next (find-if #'identity (the list (getf value :full)) :key #'fourth))

      (let ((partial (getf value :partial)))
	(setf part
	      (loop for elm in partial
		 when (or (not next) (and (< (the fixnum (second (car (second elm)))) (the fixnum (second next)))
					  (<= (the fixnum (first (car (second elm)))) (the fixnum (first next)))))
		 collect (car (second elm)))))))

  (return-from find-next-waiting-lock (if part part (list next))))

(defun release-lock (object property
		     &key (test #'eq)
		     &aux (object-lock nil) (thread (bordeaux-threads:current-thread)))
  "Releases the lock held by the current thread on *OBJECT* and *PROPERTY*. 

Relates to *ACQUIRE-LOCK*"
  (declare (type function test))
  
  (bordeaux-threads:with-lock-held (*function-lock*)
    (multiple-value-bind (value found) (gethash object *locks-held*)
      (if found ;; (and found value)
	  (setf object-lock value)
	  (progn
	    (log:error "No lock for object ~a exists!" object)
	    (cerror "Ignore and continue." "No lock for object exists!")
	    (return-from release-lock nil)))

      (if property
	  (let ((entry (cadar (member property (getf object-lock :partial) :key #'first :test test))))
	    (if (null entry)
		(progn
		  (log:error "No lock for property ~a exists!" property)
		  (cerror "Ignore and continue." "No lock for property exists!")
		  (return-from release-lock nil))

		(trivial-utilities:aif (car (member thread entry :key #'third))
				       (when (<= (decf (the fixnum (nth 4 it))) 0)
					 (trivial-utilities:aif (delete-if #'(lambda (x) (eq x thread)) entry :key #'third)
								(setf (cadar (member property (getf object-lock :partial) :key #'first :test test)) it)
								(setf (getf object-lock :partial) (delete-if #'(lambda (x) (eq x property)) (getf object-lock :partial) :key #'first))))
				       (progn
					 (log:error "No lock for thread ~a exists!" thread)
					 (cerror "Ignore and continue." "No lock for thread exists!")
					 (return-from release-lock nil)))))
	  
	  (trivial-utilities:aif (car (member thread (getf object-lock :full) :key #'third))
				 (when (<= (decf (the fixnum (nth 4 it))) 0)
				   (setf (getf object-lock :full)
					 (delete-if #'(lambda (x) (eq x thread)) (getf object-lock :full) :key #'third)))
				 (progn
				   (log:error "No lock for thread ~a exists!" thread)
				   (cerror "Ignore and continue." "No lock for thread exists!")
				   (return-from release-lock nil))))

      (if (and (null (getf object-lock :partial))
	       (null (getf object-lock :full)))
	  (remhash object *locks-held*)
	  (loop for elm in (find-next-waiting-lock object)
	     when (fourth elm)
	     do (let ((cond-var (fourth elm)))
		  (setf (fourth elm) nil) ;; Every thread that is (potentially) running, has no conditional variable
		  (bordeaux-threads:condition-notify cond-var))))))

  (return-from release-lock nil))

(defmacro with-object-lock-held ((object &key (test #'eq) (property nil) (timeout nil)) &body body)
  "Supporting macro to simplify usage of *ACQUIRE-LOCK* and *RELEASE-LOCK*."
  (trivial-utilities:once-only (object property)
    `(progn
       (acquire-lock ,object ,property :timeout ,timeout :test ,test)
       (unwind-protect
	    (progn
	      ,@body)
	 (release-lock ,object ,property :test ,test)))))



