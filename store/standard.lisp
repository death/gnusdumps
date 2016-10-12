;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/store/standard
  (:use #:cl #:gnusdumps/protocols #:gnusdumps/reopen-mixin)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:export
   #:standard-store))

(in-package #:gnusdumps/store/standard)

(defclass standard-store (reopen-mixin store)
  ())
