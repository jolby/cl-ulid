(defpackage :ulid/tests
  (:use #:cl #:fiveam #:base-32 #:ulid)
  #+nil(:import-from #:fiveam
                #:run-tests)
  (:export #:ulid/tests-suite))
