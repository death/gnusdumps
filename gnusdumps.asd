;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:gnusdumps
  :description "Create article dumps for reading with GNUS"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("gnusdumps/all"))
