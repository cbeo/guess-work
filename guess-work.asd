;;;; guess-work.asd

(asdf:defsystem #:guess-work
  :description "Describe guess-work here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
               (:file "macros")
               (:file "guess-work")))
