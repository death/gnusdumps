;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/convenience
  (:use #:cl #:gnusdumps/protocols)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:export
   #:with-open-objects))

(in-package #:gnusdumps/convenience)

(defmacro with-open-objects ((&rest objects) &body forms)
  (if (null objects)
      `(progn ,@forms)
      (destructuring-bind (object &rest more-objects) objects
        `(progn
           (open ,object)
           (unwind-protect
                (with-open-objects (,@more-objects)
                  ,@forms)
             (close ,object))))))
