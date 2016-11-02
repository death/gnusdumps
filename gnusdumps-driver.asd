;;;; +----------------------------------------------------------------+
;;;; | gnusdumps driver                                               |
;;;; +----------------------------------------------------------------+

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages :cl-html5-parser '(:html5-parser))

(asdf:defsystem #:gnusdumps-driver
  :description "Generic driver for use with gnusdumps"
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("gnusdumps/driver/all"))
