;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/store/transient
  (:use #:cl #:gnusdumps/protocols #:gnusdumps/store/standard)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:export
   #:transient-store))

(in-package #:gnusdumps/store/transient)

(defclass entry ()
  ((file-number :initarg :file-number :reader file-number)
   (digest :initarg :digest :accessor digest)))

(defclass transient-store (standard-store)
  ((table :initform (make-hash-table :test 'equal))
   (file-number-counter :initform -1)))

(defmethod open ((object transient-store)))

(defmethod close ((object transient-store)))

(defmethod reset ((object transient-store))
  (with-slots (table file-number-counter) object
    (clrhash table)
    (setf file-number-counter -1))
  object)

(defmethod member-p ((article article) (store transient-store))
  (with-slots (table) store
    (let ((entry (gethash (id article) table)))
      (cond ((null entry)
             (values nil nil))
            ((equalp (digest article) (digest entry))
             (values (file-number entry) nil))
            (t
             (values (file-number entry) t))))))

(defmethod create ((article article) (store transient-store))
  (assert (not (member-p article store)))
  (with-slots (table file-number-counter) store
    (let ((digest (digest article))
          (file-number (incf file-number-counter)))
      (setf (gethash (id article) table)
            (make-instance 'entry
                           :digest digest
                           :file-number file-number))
      file-number)))

(defmethod update ((article article) (store transient-store))
  (assert (member-p article store))
  (with-slots (table) store
    (let ((entry (gethash (id article) table)))
      (setf (digest entry) (copy-seq (digest article)))
      (file-number entry))))
