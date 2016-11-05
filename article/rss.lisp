;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/article/rss
  (:use #:cl #:gnusdumps/article/standard)
  (:export
   #:rss-article
   #:link))

(in-package #:gnusdumps/article/rss)

(defclass rss-article (standard-article)
  ((link :initarg :link :reader link)))

(defmethod link ((article standard-article))
  nil)
