(in-package :cl-user)

(defpackage cl-ulid-asd
  (:use :cl :asdf))
(in-package :cl-ulid-asd)

(defsystem "cl-ulid/insecure-core"
  :version "0.1.0"
  :description "ULID implementation in Common Lisp. Core uses lisp implementation of randomness."
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("cl-intbytes")
  :serial t
  :components
  ((:module "src"
   :components
   ((:file "package")
    (:file "base32")
    (:file "ulid")))))

(defsystem "cl-ulid"
  :version "0.1.0"
  :description "ULID implementation in Common Lisp. Uses ironclad for randomness."
  :author "Joel Boehland <jboehland@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("ironclad" "cl-ulid/insecure-core")
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "ironclad-random"))))
  :in-order-to ((test-op (load-op "cl-ulid/tests")))
  :perform (test-op (op c)
                    (unless
                        (uiop:symbol-call
                         :fiveam :run!
                         (uiop:find-symbol* :ulid/tests-suite
                                            :ulid/tests))
                      (error "test failure"))))

(defsystem "cl-ulid/tests"
  :depends-on ("fiveam" "cl-ulid")
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "ulid-tests"))))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* :ulid/tests-suite
                                        :ulid/tests))))
