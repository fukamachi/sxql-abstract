(in-package #:cl-user)
(defpackage #:sxql-abstract/type
  (:use #:cl)
  (:import-from #:sxql-abstract/config
                #:*database-type*)
  (:export #:normalize-serial-type))
(in-package #:sxql-abstract/type)

(defun get-sql-type (type)
  (etypecase type
    (cons (first type))
    (keyword type)))

(defun set-sql-type (type new-type)
  (etypecase type
    (cons (cons new-type (rest type)))
    (keyword new-type)))

(defun normalize-serial-type (type auto-increment)
  (case *database-type*
    (:mysql
     (values (case (get-sql-type type)
               (:serial
                (setf auto-increment t)
                (set-sql-type type :integer))
               (:bigserial
                (setf auto-increment t)
                (set-sql-type type :bigint))
               (t type))
             auto-increment))
    (:postgres
     (if auto-increment
         (values (case (get-sql-type type)
                   (:integer (set-sql-type type :serial))
                   (:bigint (set-sql-type type :bigserial))
                   (otherwise
                    (error "Invalid PostgreSQL serial type: ~S" type)))
                 nil)
         (values type nil)))
    (otherwise
     (values type auto-increment))))
