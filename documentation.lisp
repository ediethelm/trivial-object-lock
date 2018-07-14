;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-object-lock)

(defsection @trivial-object-lock-manual (:title "Trivial Object Lock Manual")
  "[![pipeline status](https://gitlab.com/ediethelm/trivial-object-lock/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-object-lock/commits/master)"
  (@trivial-object-lock-description section)
  (@trivial-object-lock-installing section)
  (@trivial-object-lock-example section)
  (@trivial-object-lock-exported section)
  (@trivial-object-lock-license section)
  (@trivial-object-lock-contributing section))


(defsection @trivial-object-lock-description (:title "Description")

"This library allows locking access to any SYMBOL on two different levels: *FULL-LOCK* and *PARTIAL-LOCK*.

A *FULL-LOCK* blocks any access to the *SYMBOL*.  
A *PARTIAL-LOCK* blocks access to a property of the *SYMBOL*. A property can be a slot (in the case of a class instance) or any other *SYMBOL*.

*PARTIAL-LOCK*s allow simultameous access to different properties of a *SYMBOL*, while being superseeded by any *FULL-LOCK*.")

(defsection @trivial-object-lock-installing (:title "Installing trivial-object-lock")
  "Since this project is not yet available in the latest [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") distribution, it has to be copied to your local-projects folder:

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-object-lock.git
```

After the files are copied, we can use [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") to load trivial-object-lock:

```lisp
(ql:quickload :trivial-object-lock)
```
")

(defsection @trivial-object-lock-example (:title "Working Example")
  "The following code demonstrates the use-case for this library.  
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
")

(defsection @trivial-object-lock-exported (:title "Exported Symbols")
  (interrupt-execution condition)
  (lock-timeout condition)
  (acquire-lock function)
  (release-lock function)
  (with-object-lock-held macro))

(defsection @trivial-object-lock-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-object-lock/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-object-lock-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-object-lock/blob/master/CONTRIBUTING.md 'Contributing') document for more information.")
