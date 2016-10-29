(in-package #:cl-user)
(defpackage #:sxql-abstract/config
  (:use #:cl)
  (:export #:*database-type*
           #:with-database-type))
(in-package #:sxql-abstract/config)

(defvar *database-type*)

(defmacro with-database-type (type &body body)
  `(let* ((*database-type* ,type)
          (sxql:*quote-character* (case *database-type*
                                    (:mysql #\`)
                                    ((:postgres :sqlite3) #\")
                                    (otherwise nil))))
     ,@body))
