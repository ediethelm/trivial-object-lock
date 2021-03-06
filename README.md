# Trivial Object Lock Manual

###### \[in package TRIVIAL-OBJECT-LOCK\]
[![pipeline status](https://gitlab.com/ediethelm/trivial-object-lock/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-object-lock/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-object-lock.svg)](http://quickdocs.org/trivial-object-lock/)
[![coverage report](https://gitlab.com/ediethelm/trivial-object-lock/badges/master/coverage.svg?job=sbcl-test)](https://gitlab.com/ediethelm/trivial-object-lock/-/jobs/artifacts/master/browse?job=sbcl-test)

## Description

This library allows locking access to any SYMBOL on two different levels: *FULL-LOCK* and *PARTIAL-LOCK*.

A *FULL-LOCK* blocks any access to the *SYMBOL*.  
A *PARTIAL-LOCK* blocks access to a property of the *SYMBOL*. A property can be a slot (in the case of a class instance) or any other *SYMBOL*.

*PARTIAL-LOCK*s allow simultameous access to different properties of a *SYMBOL*, while being superseeded by any *FULL-LOCK*.

## Installing trivial-object-lock

This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-object-lock)
```


## Working Example

The following code demonstrates the use-case for this library.  
Note: The sequence '=>' indicates the result from evaluating the previous expression.

```lisp
  (defclass something ()
    (a-slot
     another-slot))

(let ((result '())
       (object (make-instance 'something)))

    (acquire-lock object 'a-slot)
    
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property 'another-slot) (sleep 1) (push 'A result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object) (sleep 1) (push 'B result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property 'a-slot) (sleep 1) (push 'C result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property 'another-slot) (sleep 1) (push 'D result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object :property 'another-slot)  (sleep 1) (push 'E result))))
    (bordeaux-threads:make-thread #'(lambda () (with-object-lock-held (object) (sleep 1) (push 'F result))))
    
    (sleep 2)
    (release-lock object 'a-slot)
    (sleep 8)
    
    result)

=> (F E C D B A) or (F E D C B A) depending on thread timming
```


## Exported Symbols

- [condition] INTERRUPT-EXECUTION

- [condition] LOCK-TIMEOUT

- [function] ACQUIRE-LOCK OBJECT PROPERTY &KEY (BLOCKING T) (TIMEOUT NIL) (TEST #'EQ) &AUX (OBJECT-LOCK NIL) (COND-VAR NIL) (THREAD (BORDEAUX-THREADS:CURRENT-THREAD))

    Acquire a lock on the given *OBJECT*. When not *NIL*, *PROPERTY* will be used to create a *PARTIAL-LOCK* on *OBJECT*; otherwise a *FULL-LOCK* is created.
    
    Returns *NIL* if the lock was acquired; otherwise it returns a conditional variable to wait on.

- [function] RELEASE-LOCK OBJECT PROPERTY &KEY (TEST #'EQ) &AUX (OBJECT-LOCK NIL) (THREAD (BORDEAUX-THREADS:CURRENT-THREAD))

    Releases the lock held by the current thread on *OBJECT* and *PROPERTY*. 
    
    Relates to *ACQUIRE-LOCK*

- [macro] WITH-OBJECT-LOCK-HELD (OBJECT &KEY (TEST) (PROPERTY) (TIMEOUT)) &BODY BODY

    Supporting macro to simplify usage of *ACQUIRE-LOCK* and *RELEASE-LOCK*.

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-object-lock/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-object-lock/blob/master/CONTRIBUTING.md "Contributing") document for more information.
