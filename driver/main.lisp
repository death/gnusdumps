;;;; +----------------------------------------------------------------+
;;;; | gnusdumps driver                                               |
;;;; +----------------------------------------------------------------+

;; The style of this program is a bit weird, but I like it :)
;;
;; Subclassers of CONTEXT have to implement methods for these generic
;; functions:
;;
;; - FETCH-DOCUMENT-URLS
;;
;; - MAKE-ID (but see ID-GENERATION-MIXIN)
;;
;; - MAKE-SUBJECT
;;
;; - MAKE-BODY
;;
;; - DUMP-ARTICLES (but see NNEETHING-DUMP-MIXIN)
;;
;; The rest have default implementations that can be overriden.
;;
;; There are also a bunch of useful mixins.
;;

(defpackage #:gnusdumps/driver/main
  (:use #:cl)
  (:import-from #:gnusdumps)
  (:import-from #:log4cl)
  (:import-from #:dexador)
  (:import-from #:html5-parser #:parse-html5 #:element-attribute)
  (:import-from #:sqlite #:with-open-database)
  (:export
   #:context
   #:leech)
  (:export
   #:fetch-document-urls
   #:fetch-documents
   #:documents-to-articles
   #:dump-articles)
  (:export
   #:fetch-document)
  (:export
   #:document-to-article)
  (:export
   #:make-id
   #:make-parent-id
   #:make-author
   #:make-subject
   #:make-body
   #:make-date)
  (:export
   #:sleep-policy-mixin
   #:fetch-document-seconds)
  (:export
   #:logging-mixin)
  (:export
   #:id-generation-mixin)
  (:export
   #:debugging-mixin
   #:intermediate-results)
  (:export
   #:nneething-dump-mixin
   #:store-filename
   #:store-table-name
   #:dump-output-directory
   #:dump-name))

(in-package #:gnusdumps/driver/main)


;;;; The big picture

(defclass context ()
  ((name
    :initarg :name
    :reader name
    :documentation "The context's name; used to derive other names.")))

(defgeneric leech (context)
  (:documentation "Fetch documents, convert to articles, and dump."))


;;;; Default implementation

;; Utilities

(defun robust-mapcar (function list &rest more-lists)
  "Like MAPCAR, but for each set of elements have a CONTINUE restart
that allows skipping it in case of an error."
  (let ((bad-entry (cons nil nil)))
    (flet ((process (&rest items)
             (with-simple-restart (continue "Skip it")
               (return-from process
                 (apply function items)))
             bad-entry))
      (remove bad-entry (apply #'mapcar #'process list more-lists)))))

(defun read-new-value ()
  "Read and evaluate a new value of something."
  (format t "Enter a new value: ")
  (force-output)
  (multiple-value-list (eval (read))))

(defmacro metalist (lambda-list docstring form list)
  "Macroexpand and evaluate for each argument list in LIST."
  (let ((name (gensym)))
    `(macrolet ((,name ,lambda-list ,docstring ,form))
       ,@(mapcar (lambda (arglist) `(,name ,@arglist)) list))))

;; LEECH

(defgeneric fetch-document-urls (context)
  (:documentation "Retrieve a list of URLs pointing to documents to
download."))

(defgeneric fetch-documents (context urls)
  (:documentation "Fetch the documents pointed to by URLs."))

(defgeneric documents-to-articles (context documents)
  (:documentation "Convert the HTML documents to gnusdumps articles."))

(defgeneric dump-articles (context articles)
  (:documentation "Dump articles using gnusdumps."))

(defmethod leech ((context context))
  (reduce (lambda (items function)
            (funcall function context items))
          (list #'fetch-documents
                #'documents-to-articles
                #'dump-articles)
          :initial-value (fetch-document-urls context)))

;; FETCH-DOCUMENTS

(defgeneric fetch-document (context url)
  (:documentation "Fetch a document pointed to by URL."))

(defmethod fetch-documents ((context context) urls)
  (robust-mapcar (lambda (url) (fetch-document context url)) urls))

;; FETCH-DOCUMENT

(defmethod fetch-document ((context context) url)
  (parse-html5 (dex:get url)))

;; DOCUMENTS-TO-ARTICLES

(defgeneric document-to-article (context document)
  (:documentation "Convert the document to a gnusdumps article."))

(defmethod documents-to-articles ((context context) documents)
  (robust-mapcar (lambda (document) (document-to-article context document)) documents))

;; DOCUMENT-TO-ARTICLE

(defgeneric make-id (context document)
  (:documentation "Construct an ID for the article."))

(defgeneric make-parent-id (context document)
  (:documentation "Construct a parent ID for the article.  May return
NIL for an orphan."))

(defgeneric make-author (context document)
  (:documentation "Construct an author for the article."))

(defgeneric make-subject (context document)
  (:documentation "Construct a subject line for the article."))

(defgeneric make-body (context document)
  (:documentation "Construct a body for the article."))

(defgeneric make-date (context document)
  (:documentation "Construct a date for the article."))

(defmethod document-to-article ((context context) document)
  (make-instance 'gnusdumps:standard-article
                 :id (make-id context document)
                 :parent-id (make-parent-id context document)
                 :author (make-author context document)
                 :subject (make-subject context document)
                 :date (make-date context document)
                 :body (make-body context document)))

(metalist (name)
          "Set up a USE-VALUE restart around MAKE-XXX functions."
          `(defmethod ,name :around ((context context) document)
             (declare (ignore document))
             (restart-case
                 (call-next-method)
               (use-value (new-value)
                 :report ,(format nil "Return a new value from ~S." name)
                 :interactive read-new-value
                 new-value)))
          ((make-id)
           (make-parent-id)
           (make-author)
           (make-subject)
           (make-body)
           (make-date)))

;; MAKE-PARENT-ID

(defmethod make-parent-id ((context context) document)
  (declare (ignore document))
  nil)

;; MAKE-AUTHOR

(defmethod make-author ((context context) document)
  (declare (ignore document))
  (with-slots (name) context
    name))

;; MAKE-DATE

(defmethod make-date ((context context) document)
  (declare (ignore document))
  nil)


;;;; Sleep policy mixin

(defclass sleep-policy-mixin ()
  ((fetch-document-seconds
    :initarg :fetch-document-seconds
    :reader fetch-document-seconds
    :documentation "Number of seconds to wait before fetching a document."))
  (:default-initargs :fetch-document-seconds 5)
  (:documentation "A mixin to sleep a while before fetching documents."))

(defmethod fetch-document :before ((context sleep-policy-mixin) url)
  (declare (ignore url))
  (with-slots (fetch-document-seconds) context
    (sleep fetch-document-seconds)))


;;;; Logging mixin

(defclass logging-mixin ()
  ()
  (:documentation "A mixin to log fetching of documents."))

(defmethod fetch-documents :before ((context logging-mixin) urls)
  (log:debug "~D documents to fetch" (length urls)))

(defmethod fetch-document :before ((context logging-mixin) url)
  (log:debug "Fetching document ~A" url))


;;;; ID generation mixin

(defclass id-generation-mixin ()
  ((counter :initform 0))
  (:documentation "A mixin to automatically generate IDs for
articles."))

(defmethod make-id ((context id-generation-mixin) document)
  (declare (ignore document))
  (with-slots (counter) context
    (incf counter)))


;;;; Debugging mixin

(defclass debugging-mixin ()
  ((intermediate-results
    :initform '()
    :documentation "A plist that will contain intermediate results."
    :reader intermediate-results))
  (:documentation "A mixin that stores intermediate results - document
URLs, documents, and articles."))

(defmethod leech :before ((context debugging-mixin))
  (with-slots (intermediate-results) context
    (setf intermediate-results '())))

(defmethod fetch-documents :before ((context debugging-mixin) urls)
  (with-slots (intermediate-results) context
    (setf (getf intermediate-results :urls) urls)))

(defmethod documents-to-articles :before ((context debugging-mixin) documents)
  (with-slots (intermediate-results) context
    (setf (getf intermediate-results :documents) documents)))

(defmethod dump-articles :before ((context debugging-mixin) articles)
  (with-slots (intermediate-results) context
    (setf (getf intermediate-results :articles) articles)))


;;;; nneething Dump mixin with an SQLite store

(defclass nneething-dump-mixin ()
  ((base-directory
    :initarg :base-directory
    :reader base-directory
    :documentation "Base directory for storage."))
  (:documentation "nneething Dump mixin with an SQLite store."))

(defgeneric dump-output-directory (context)
  (:documentation "Construct a dump output directory pathname."))

(defgeneric dump-name (context)
  (:documentation "Construct a dump name."))

(defgeneric store-filename (context)
  (:documentation "Construct a store filename."))

(defgeneric store-table-name (context)
  (:documentation "Construct a store table name."))

(defmethod dump-articles ((context nneething-dump-mixin) articles)
  (let ((store-filename (store-filename context)))
    (ensure-directories-exist store-filename)
    (ensure-directories-exist (dump-output-directory context))
    (with-open-database (db store-filename)
      (gnusdumps:write articles
                       (make-instance 'gnusdumps:sqlite-store
                                      :handle db
                                      :table-name (store-table-name context))
                       (make-instance 'gnusdumps:nneething-dump
                                      :name (dump-name context)
                                      :base-directory (dump-output-directory context))))))

(defmethod dump-output-directory ((context nneething-dump-mixin))
  (with-slots (base-directory) context
    (merge-pathnames (make-pathname :directory '(:relative "dump"))
                     base-directory)))

(defmethod dump-name ((context nneething-dump-mixin))
  (with-slots (name) context
    name))

(defmethod store-filename ((context nneething-dump-mixin))
  (with-slots (base-directory name) context
    (make-pathname :name "store"
                   :type "db"
                   :defaults base-directory)))

(defmethod store-table-name ((context nneething-dump-mixin))
  (with-slots (name) context
    name))
