;;;; +----------------------------------------------------------------+
;;;; | gnusdumps                                                      |
;;;; +----------------------------------------------------------------+

(defpackage #:gnusdumps/store/sqlite
  (:use #:cl #:gnusdumps/protocols #:gnusdumps/store/standard)
  (:shadowing-import-from #:gnusdumps/protocols #:open #:close #:write)
  (:import-from #:sqlite #:execute-non-query #:execute-to-list
                #:last-insert-rowid #:connect #:disconnect)
  (:export
   #:sqlite-store
   #:file-name
   #:table-name))

(in-package #:gnusdumps/store/sqlite)

(defclass sqlite-store (standard-store)
  ((file-name :initarg :file-name :reader file-name)
   (table-name :initarg :table-name :reader table-name)
   (handle)
   (statements)))

(defmethod open ((object sqlite-store))
  (with-slots (file-name table-name handle statements) object
    (setf handle (connect file-name))
    (table-command object :create)
    (setf statements (prepare-statements table-name))))

(defmethod close ((object sqlite-store))
  (with-slots (handle) object
    (disconnect handle)
    (setf handle nil)))

(defmethod reset ((object sqlite-store))
  (table-command object :drop)
  (table-command object :create))

(defun table-command (store command)
  (with-slots (handle table-name) store
    (flet ((e (control-string &rest format-args)
             (execute-non-query handle (apply #'format nil control-string format-args))))
      (ecase command
        (:create
         (e "CREATE TABLE IF NOT EXISTS ~A (article_id INTEGER, article_digest BLOB)" table-name)
         (e "CREATE UNIQUE INDEX IF NOT EXISTS ~A_idx ON ~A (article_id)" table-name table-name))
        (:drop
         (e "DROP TABLE IF EXISTS ~A" table-name)
         (e "DROP INDEX IF EXISTS ~A_idx" table-name))))))

(defun prepare-statements (table-name)
  (let ((statements (make-hash-table)))
    (flet ((p (type name control-string &rest format-args)
             (let ((sql (apply #'format nil control-string format-args)))
               (setf (gethash name statements)
                     (ecase type
                       (1 (lambda (handle &rest args)
                            (first (apply #'execute-to-list handle sql args))))
                       (0 (lambda (handle &rest args)
                            (apply #'execute-non-query handle sql args))))))))
      (p 1 :member-p "SELECT rowid, article_digest FROM ~A WHERE article_id = ?" table-name)
      (p 0 :insert "INSERT INTO ~A (article_id, article_digest) VALUES (?, ?)" table-name)
      (p 0 :update "UPDATE ~A SET article_digest = ? WHERE article_id = ?" table-name))
    statements))

(defun run-statement (store name &rest args)
  (with-slots (handle statements) store
    (let ((statement (gethash name statements)))
      (if (null statement)
          (error "No statement with name ~S." name)
          (apply statement handle args)))))

(defmethod member-p ((article article) (store sqlite-store))
  (let ((row (run-statement store :member-p (id article))))
    (cond ((null row)
           (values nil nil))
          ((equalp (cadr row) (digest article))
           (values (car row) nil))
          (t
           (values (car row) t)))))

(defmethod create ((article article) (store sqlite-store))
  (assert (integerp (id article)))
  (with-slots (handle) store
    (run-statement store :insert (id article) (digest article))
    (last-insert-rowid handle)))

(defmethod update ((article article) (store sqlite-store))
  (assert (integerp (id article)))
  (run-statement store :update (digest article) (id article))
  (values (member-p article store)))

(defmethod need-update ((article article) (store sqlite-store))
  (assert (integerp (id article)))
  (run-statement store :update (fill (digest article) 0) (id article))
  (values))
