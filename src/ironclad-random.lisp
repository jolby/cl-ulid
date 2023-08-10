(in-package :cl-user)

(defpackage ulid-ironclad-random
  (:use :cl)
  (:import-from :ironclad
   :random-data
   :strong-random)
  (:import-from #:ulid
                #:*random-bytes-fn*
                #:*random-number-fn*
                #:*default-ulid-generator*
                #:make-ulid-generator))

(in-package :ulid-ironclad-random)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *random-bytes-fn* #'ironclad:random-data
        *random-number-fn* #'ironclad:strong-random)
  (when (null *default-ulid-generator*)
    (setf *default-ulid-generator* (make-ulid-generator))))
