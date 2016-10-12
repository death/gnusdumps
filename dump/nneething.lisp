;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/dump/nneething
  (:use #:cl #:gnusdumps/protocols #:gnusdumps/dump/standard)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:import-from #:local-time #:format-rfc1123-timestring)
  (:import-from #:log4cl)
  (:export
   #:nneething-dump
   #:base-directory
   #:name))

(in-package #:gnusdumps/dump/nneething)

(defclass nneething-dump (standard-dump)
  ((base-directory :initarg :base-directory :reader base-directory)
   (name :initarg :name :reader name)))

(defmethod open :after ((object nneething-dump))
  (with-slots (base-directory) object
    (ensure-directories-exist base-directory)))

(defmethod reset ((object nneething-dump))
  (with-slots (base-directory) object
    (ensure-directories-exist base-directory)
    ;; Yeah, I don't think so...
    #+nil
    (dolist (file (directory
                   (make-pathname :name :wild :type :wild
                                  :defaults base-directory)))
      (delete-file file)))
  object)

(defmethod write-using-file-number ((article article) (file-number integer) (dump nneething-dump))
  (log:debug "Dumping article ~A to file ~A~%" (id article) file-number)
  (with-slots (base-directory) dump
    (let ((filename (make-pathname :name (format nil "~8,'0D" file-number)
                                   :defaults base-directory)))
      (with-open-file (stream filename :direction :output :if-exists :supersede)
        (format stream "From: ~A~%" (author article))
        (format stream "Subject: ~A~%" (subject article))
        (format stream "Message-ID: ~A~%" (message-id (id article) dump))
        (when (parent-id article)
          (format stream "In-Reply-To: ~A~%" (message-id (parent-id article) dump)))
        (when (date article)
          (format stream "Date: ~A~%" (format-rfc1123-timestring nil (date article))))
        (format stream "Lines: ~D~%" (line-count (body article)))
        (format stream "~%")
        (format stream "~A~%" (body article))))))

(defun message-id (article-id dump)
  (with-slots (name) dump
    (format nil "<~A@~A>" article-id name)))

(defun line-count (string)
  (1+ (count #\Newline string)))
