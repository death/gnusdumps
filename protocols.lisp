;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/protocols
  (:use #:cl)
  (:shadow #:open #:close #:write)
  (:export
   #:article
   #:id
   #:parent-id
   #:author
   #:subject
   #:body
   #:date
   #:digest
   #:open
   #:close
   #:reset
   #:store
   #:member-p
   #:create
   #:update
   #:transaction
   #:dump
   #:write))

(in-package #:gnusdumps/protocols)

(defclass article ()
  ())

(defgeneric id (article))
(defgeneric parent-id (article))
(defgeneric author (article))
(defgeneric subject (article))
(defgeneric body (article))
(defgeneric date (article))
(defgeneric digest (article))

(defgeneric open (object))
(defgeneric close (object))
(defgeneric reset (object))

(defclass store ()
  ())

(defgeneric member-p (article store))
(defgeneric create (article store))
(defgeneric update (article store))
(defgeneric transaction (function store))

(defclass dump ()
  ())

(defgeneric write (article store dump))
