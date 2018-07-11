;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-object-lock)

(defsection @trivial-object-lock-manual (:title "Trivial Object Lock Manual")
  "Describe :PARTIAL und :FULL locks"
  (interrupt-execution condition)
  (lock-timeout condition)
  (acquire-lock function)
  (release-lock function)
  (with-object-lock-held macro)
  ;; this is a subsection
  (@trivial-object-lock-examples section))
