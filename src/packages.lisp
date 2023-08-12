(defpackage :base-32
  (:use :cl)
  (:import-from #:nibbles
                #:ub48ref/be
                #:ub48set/be
                #:ub80ref/be
                #:ub80set/be)
  (:export
   ;; Constants
   #:+time-max+
   #:+randomness-max+
   #:+timestamp-bytes-len+
   #:+randomness-bytes-len+
   #:+ulid-bytes-len+
   #:+timestamp-string-len+
   #:+randomness-string-len+
   #:+ulid-string-len+
   #:+encode-lut+
    #:+decode-lut+
   ;; Types
   #:timestamp-byte-array
   #:randomness-byte-array
   #:ulid-byte-array
   #:timestamp-integer
   #:randomness-integer
   ;;Functions
   #:make-timestamp-bytes-array
   #:make-randomness-bytes-array
   #:make-ulid-bytes-array
   #:make-timestamp-string-array
   #:make-randomness-string-array
   #:make-ulid-string-array
   #:timestamp->octets
   #:randomness->octets
   #:encode-timestamp-bytes
   #:encode-randomness-bytes
   #:encode-ulid-bytes
   #:encode-timestamp-and-randomness-bytes
   #:decode-timestamp-to-bytes
   #:decode-randomness-to-bytes
   #:decode-ulid-to-bytes
   #:decode-to-values))

(defpackage :ulid
  (:use :cl)
  (:import-from #:nibbles
                #:ub48set/be
                #:ub80set/be)
  (:import-from #:base-32
                #:+time-max+
                #:+randomness-max+
                #:+timestamp-bytes-len+
                #:timestamp-integer
                #:randomness-integer
                #:make-ulid-bytes-array
                #:make-ulid-string-array
                #:encode-ulid-bytes)
  (:export
   #:*random-bytes-fn*
   #:*random-number-fn*
   #:*default-ulid-generator*
   #:*ulid-generator*
   #:get-unix-time-ms
   #:make-ulid-generator
   #:ulid))
