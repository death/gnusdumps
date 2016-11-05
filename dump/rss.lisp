;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/dump/rss
  (:use #:cl
        #:gnusdumps/protocols
        #:gnusdumps/dump/standard
        #:gnusdumps/article/rss)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:import-from #:local-time #:format-rfc3339-timestring #:now)
  (:import-from #:cxml #:make-octet-stream-sink)
  (:import-from #:cxml-xmls #:map-node)
  (:export
   #:rss-dump
   #:output-filename))

(in-package #:gnusdumps/dump/rss)


;;;; RSS dump

(defclass rss-dump (standard-dump)
  ((output-filename :initarg :output-filename :reader output-filename)
   (channel :initform (make-instance 'channel))))

(defmethod initialize-instance :after ((dump rss-dump) &key (title "no title")
                                                            (link "http://no.link/")
                                                            (description "no description"))
  (with-slots (channel) dump
    (setf (title channel) title
          (link channel) link
          (description channel) description)))

(defmethod reset ((dump rss-dump))
  (ignore-errors (delete-file (output-filename dump)))
  (with-slots (channel) dump
    (setf (items channel) '())))

(defmethod write-using-file-number ((article article) (file-number integer) (dump rss-dump))
  (with-slots (channel) dump
    (push (make-instance 'item
                         :title (subject article)
                         :link (or (link article) (format nil "http://no.link/~D" file-number))
                         :description (body article)
                         :pub-date (format-rfc3339-timestring nil (or (date article) (now))))
          (items channel))))

(defmethod close ((dump rss-dump))
  (with-slots (channel) dump
    (write-channel channel (output-filename dump))))


;;;; Write a channel to an RSS file

(defclass channel ()
  ((title :initarg :title :accessor title)
   (link :initarg :link :accessor link)
   (description :initarg :description :accessor description)
   (items :initarg :items :accessor items))
  (:default-initargs :items '()))

(defclass item ()
  ((title :initarg :title :reader title)
   (link :initarg :link :reader link)
   (description :initarg :description :reader description)
   (pub-date :initarg :pub-date :reader pub-date)))

(defun channel->xmls (channel)
  `("rss" (("version" "2.0"))
          ("channel" ()
                     ("title" () ,(title channel))
                     ("link" () ,(link channel))
                     ("description" () ,(description channel))
                     ,@(mapcar (lambda (item)
                                 `("item" ()
                                          ("title" () ,(title item))
                                          ("link" () ,(link item))
                                          ("description" () ,(description item))
                                          ("pubDate" () ,(pub-date item))))
                               (items channel)))))

(defun write-channel (channel filename)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (map-node (make-octet-stream-sink stream)
              (channel->xmls channel)
              :include-namespace-uri nil)))
