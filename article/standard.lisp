;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/article/standard
  (:use #:cl #:gnusdumps/protocols)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:import-from #:ironclad #:make-digesting-stream #:produce-digest)
  (:import-from #:local-time #:timestamp #:timestamp-to-universal)
  (:import-from #:babel #:string-to-octets)
  (:export
   #:standard-article))

(in-package #:gnusdumps/article/standard)

(defclass standard-article (article)
  ((id :initarg :id :reader id)
   (parent-id :initarg :parent-id :reader parent-id)
   (author :initarg :author :reader author)
   (subject :initarg :subject :reader subject)
   (body :initarg :body :reader body)
   (date :initarg :date :reader date)
   (digest-cache :initform nil))
  (:default-initargs :parent-id nil))

(defmethod print-object ((object standard-article) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id) object
      (format stream "id:~A" id)))
  object)

(defmethod digest ((article standard-article))
  (with-slots (digest-cache) article
    (or digest-cache
        (setf digest-cache
              (let ((stream (make-digesting-stream :sha256)))
                (labels ((add (x)
                           (etypecase x
                             (string
                              (write-byte 0 stream)
                              (add (string-to-octets x)))
                             (integer
                              (check-type x (unsigned-byte 64))
                              (write-byte 1 stream)
                              (do ((i 0 (+ i 8)))
                                  ((= i 64))
                                (write-byte (ldb (byte 8 i) x) stream)))
                             (simple-array
                              (write-byte 2 stream)
                              (write-sequence x stream))
                             (null
                              (write-byte 3 stream))
                             (timestamp
                              (write-byte 4 stream)
                              (add (timestamp-to-universal x))))))
                  (add (id article))
                  (add (parent-id article))
                  (add (author article))
                  (add (subject article))
                  (add (body article))
                  (add (date article)))
                (produce-digest stream))))))
