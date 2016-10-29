(in-package #:cl-user)
(defpackage #:sxql-abstract/util
  (:use #:cl)
  (:export #:lispify
           #:unlispify
           #:ensure-string
           #:ensure-list
           #:unlispify-sql-symbol))
(in-package #:sxql-abstract/util)

(defun escaped-symbol-p (symbol)
  (declare (optimize speed)
           (type symbol symbol))
  (not (string= symbol (string-upcase symbol))))

(defun symbol-name-literally (symbol)
  (check-type symbol symbol)
  (if (escaped-symbol-p symbol)
      (symbol-name symbol)
      (string-downcase symbol)))

(defun lispify (object)
  (check-type object string)
  (substitute #\- #\_ object))

(defun unlispify (object)
  (check-type object string)
  (substitute #\_ #\- object))

(defun ensure-string (object)
  (etypecase object
    (string object)
    (symbol (symbol-name-literally object))))

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defun unlispify-sql-symbol (object)
  (etypecase object
    (symbol (sxql:make-sql-symbol (unlispify (symbol-name-literally object))))
    (string (sxql:make-sql-symbol (unlispify object)))))
