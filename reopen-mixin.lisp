;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/reopen-mixin
  (:use #:cl #:gnusdumps/protocols)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:export
   #:reopen-mixin))

(in-package #:gnusdumps/reopen-mixin)

(defclass reopen-mixin ()
  ((counter :initform 0)))

(defmethod open :around ((object reopen-mixin))
  (with-slots (counter) object
    (when (zerop counter)
      (call-next-method))
    (incf counter)))

(defmethod close :around ((object reopen-mixin))
  (with-slots (counter) object
    (when (plusp counter)
      (decf counter)
      (when (zerop counter)
        (call-next-method)))))

(defmethod reset :around ((object reopen-mixin))
  (open object)
  (unwind-protect (call-next-method)
    (close object)))
