;;;; package.lisp

(defpackage #:guess-work
  (:use #:cl)
  (:export #:situation
           #:label 
           #:make-rule
           #:with-situation-class
           #:run-simulation
           #:run-and-print-simulation-on-data
           #:defsituation))
