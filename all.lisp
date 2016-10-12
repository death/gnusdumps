;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(uiop:define-package #:gnusdumps/all
  (:nicknames #:gnusdumps)
  (:use-reexport #:gnusdumps/protocols
                 #:gnusdumps/convenience
                 #:gnusdumps/article/standard
                 #:gnusdumps/store/standard
                 #:gnusdumps/store/transient
                 #:gnusdumps/store/sqlite
                 #:gnusdumps/dump/standard
                 #:gnusdumps/dump/nneething))
