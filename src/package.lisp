(defpackage :ulid
  (:use :cl)
  ;; (:import-from #:cl-intbytes
  ;;               #:int->octets
  ;;               #:octets->int)
  (:import-from #:nibbles
                #:ub48ref/be
                #:ub48set/be
                #:ub80ref/be
                #:ub80set/be)
  (:export
   ;; Constants
   #:+time-max+
   #:+randomness-max+

   ;; Types
   #:timestamp-byte-array
   #:randomness-byte-array
   #:ulid-byte-array
   #:timestamp-integer
   #:randomness-integer

   #:encode-timestamp-bytes
   #:encode-randomness-bytes
   #:encode-ulid-bytes
   #:encode-timestamp-and-randomness-bytes
   #:decode-timestamp-to-bytes
   #:decode-randomness-to-bytes
   #:decode-ulid-to-bytes
   #:*random-bytes-fn*
   #:*random-number-fn*
   #:ulid
   #:make-ulid
   #:make-ulid-string
   #:ulid-timestamp))
