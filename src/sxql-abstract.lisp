(in-package #:cl-user)
(defpackage #:sxql-abstract
  (:nicknames #:sxqla)
  (:use #:cl)
  (:import-from #:sxql-abstract/type
                #:normalize-serial-type)
  (:import-from #:sxql-abstract/config
                #:*database-type*
                #:with-database-type)
  (:import-from #:sxql-abstract/util
                #:lispify
                #:unlispify
                #:ensure-string
                #:ensure-list
                #:unlispify-sql-symbol)
  (:export #:*database-type*
           #:with-database-type
           #:create-table
           #:add-column
           #:drop-column
           #:add-index
           #:drop-index
           #:add-primary-key
           #:drop-primary-key))
(in-package #:sxql-abstract)

(defun index-name (type table-name column-names)
  (let ((column-names (ensure-list column-names)))
    (format nil "~(~A~)_~A_~{~A~^_~}"
            type
            (unlispify (ensure-string table-name))
            (mapcar (lambda (index)
                      (unlispify (ensure-string index)))
                    column-names))))

(defun %create-table (table-name column-infos options)
  (let ((additional-sqls '()))
    (destructuring-bind (table-name &key if-not-exists)
        (if (consp table-name)
            table-name
            (list table-name))
      (list*
       (apply
        #'sxql:make-statement
        :create-table (list (unlispify-sql-symbol table-name)
                            :if-not-exists if-not-exists)
        (mapcar (lambda (column-info)
                  (destructuring-bind (column-name &key type primary-key not-null default auto-increment)
                      column-info
                    (multiple-value-bind (type auto-increment)
                        (normalize-serial-type type auto-increment)
                      (list (unlispify-sql-symbol column-name)
                            :type type
                            :auto-increment auto-increment
                            :primary-key primary-key
                            :not-null not-null
                            :default default))))
                column-infos)
        (mapcan (lambda (index)
                  (ecase (first index)
                    (:primary-key
                     (list
                      (sxql:primary-key
                       (format nil "~A_pkey" table-name)
                       (mapcar #'unlispify-sql-symbol (rest index)))))
                    (:unique-key
                     (list
                      (sxql:unique-key
                       ;; FIXME: it'll raise an error if the index name is too long
                       (index-name :unique table-name (rest index))
                       (mapcar (lambda (column)
                                 (unlispify (ensure-string column)))
                               (rest index)))))
                    (:key
                     (if (eq *database-type* :postgres)
                         (progn
                           (setf additional-sqls
                                 (append additional-sqls
                                         (list (sxql:create-index (sxql:make-sql-symbol
                                                                   (index-name :key table-name (rest index)))
                                                                  :on (cons (unlispify-sql-symbol table-name)
                                                                            (mapcar #'unlispify-sql-symbol (rest index)))))))
                           nil)
                         (list (sxql:index-key
                                (mapcar #'unlispify-sql-symbol (rest index))))))))
                options))
       additional-sqls))))

(defmacro create-table (table-name column-infos &body options)
  `(%create-table ,table-name
                  (list ,@(mapcar (lambda (info)
                                    `(list ',(first info) ,@(rest info)))
                                  column-infos))
                  (list ,@(mapcar (lambda (option)
                                    `(quote ,option))
                                  options))))

(defun add-column (column-name &key type auto-increment default primary-key not-null first after)
  (multiple-value-bind (type auto-increment)
      (normalize-serial-type type auto-increment)
    (sxql:make-clause :add-column
                      (unlispify-sql-symbol column-name)
                      :type type
                      :default default
                      :primary-key primary-key
                      :not-null not-null
                      :auto-increment auto-increment
                      :first (and (not (eq *database-type* :postgres))
                                  first)
                      :after (and (not (eq *database-type* :postgres))
                                  after))))

(defun drop-column (column-name)
  (sxql:drop-column (unlispify-sql-symbol column-name)))

(defun add-index (table-name column-names index-name &key unique using)
  (let ((column-names (ensure-list column-names)))
    (sxql:create-index
     (sxql:make-sql-symbol index-name)
     :unique unique
     :using using
     :on (list* (unlispify-sql-symbol table-name)
                (mapcar #'unlispify-sql-symbol column-names)))))

(defun drop-index (table-name index-name)
  (if (eq *database-type* :postgres)
      (sxql:drop-index index-name)
      (sxql:drop-index index-name
                       :on (unlispify-sql-symbol table-name))))

(defun add-primary-key (table-name column-names)
  (sxql:make-statement :alter-table (unlispify-sql-symbol table-name)
                       (apply #'sxql:make-clause :add-primary-key
                              (mapcar #'unlispify-sql-symbol column-names))))

(defun drop-primary-key (table-name)
  (sxql:make-statement :alter-table (unlispify-sql-symbol table-name)
                       (sxql:drop-primary-key)))
