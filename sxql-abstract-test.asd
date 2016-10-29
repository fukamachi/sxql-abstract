#|
  This file is a part of sxql-abstract project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-abstract-test-asd
  (:use :cl :asdf))
(in-package :sxql-abstract-test-asd)

(defsystem sxql-abstract-test
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:sxql-abstract
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "sxql-abstract"))))
  :description "Test system for sxql-abstract"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
