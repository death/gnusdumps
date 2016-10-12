;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/dump/standard
  (:use #:cl #:gnusdumps/protocols #:gnusdumps/convenience #:gnusdumps/reopen-mixin)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:export
   #:standard-dump
   #:write-using-file-number))

(in-package #:gnusdumps/dump/standard)

(defclass standard-dump (reopen-mixin dump)
  ())

(defgeneric write-using-file-number (article file-number dump))

(defmethod open ((object standard-dump)))

(defmethod close ((object standard-dump)))

(defmethod write ((article article) (store store) (dump standard-dump))
  (with-open-objects (store dump)
    (multiple-value-bind (exists modified) (member-p article store)
      (cond ((not exists)
             (write-using-file-number article
                                      (create article store)
                                      dump))
            ((not modified))
            (t
             (write-using-file-number article
                                      (update article store)
                                      dump))))))

(defmethod write ((articles list) (store store) (dump standard-dump))
  (with-open-objects (store dump)
    (dolist (article articles)
      (write article store dump))))
