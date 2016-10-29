#|
  This file is a part of sxql-abstract project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-abstract-asd
  (:use :cl :asdf))
(in-package :sxql-abstract-asd)

(defsystem sxql-abstract
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:sxql)
  :components ((:module "src"
                :components
                ((:file "sxql-abstract" :depends-on ("type" "config" "util"))
                 (:file "type" :depends-on ("config"))
                 (:file "config")
                 (:file "util"))))
  :description "An abstract layer for SQL between RDBMS"
  :in-order-to ((test-op (test-op sxql-abstract-test))))
