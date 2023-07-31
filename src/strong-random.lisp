(defpackage :ulid-ironclad-random
  (:use :cl)
  (:import-from #:crypto
                #:random-data)
  (:import-from #:ulid
                #:*random-byte-fn*))

(in-package :ulid-ironclad-random)

(setf *random-byte-fn* #'crypto:random-data)
