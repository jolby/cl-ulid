(in-package :ulid)

(defconstant +time-max+ (1- (expt 2 48)))
(defconstant +randomness-max+ (1- (expt 2 80)))

(defconstant +timestamp-bytes-len+ 6)
(defconstant +randomness-bytes-len+ 10)
(defconstant +ulid-bytes-len+ (+ +timestamp-bytes-len+ +randomness-bytes-len+))

(defconstant +timestamp-string-len+ 10)
(defconstant +randomness-string-len+ 16)
(defconstant +ulid-string-len+ (+ +timestamp-string-len+ +randomness-string-len+))

(deftype timestamp-byte-array () '(simple-array (unsigned-byte 8) (#.+timestamp-bytes-len+)))
(deftype randomness-byte-array () '(simple-array (unsigned-byte 8) (#.+randomness-bytes-len+)))
(deftype ulid-byte-array () '(simple-array (unsigned-byte 8) (#.+ulid-bytes-len+)))

(deftype timestamp-integer () '(integer 0 #.+time-max+))
(deftype randomness-integer () '(integer 0 #.+randomness-max+))

;;;; Lookup tables for encoding and decoding
;;;; The encoding and decoding arithmetics are based on the implementation of RobThree
;;;; https://github.com/RobThree/NUlid/blob/89f5a9fc827d191ae5adafe42547575ed3a47723/NUlid/Ulid.cs#L168
(defconstant +encode-lut+ "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(defconstant +decode-lut+
  #(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 0 1 2 3 4 5 6 7 8 9 255 255 255 255
  255 255 255 10 11 12 13 14 15 16 17 255 18 19 255 20 21 255 22 23 24 25 26
  255 27 28 29 30 31 255 255 255 255 255 255 10 11 12 13 14 15 16 17 255 18 19
  255 20 21 255 22 23 24 25 26 255 27 28 29 30 31 255 255 255 255 255 255 255)
  "Char to index lookup array for massive speedup since we can find a char's
 index in O(1). We use 255 as 'sentinel' value for invalid indexes. ")


(defun make-timestamp-bytes-array ()
  (make-array +timestamp-bytes-len+ :element-type '(unsigned-byte 8) :adjustable nil))

(defun make-timestamp-string-array ()
  (make-array +timestamp-string-len+ :element-type 'character :adjustable nil))

(defun make-randomness-bytes-array ()
  (make-array +randomness-bytes-len+ :element-type '(unsigned-byte 8) :adjustable nil))

(defun make-randomness-string-array ()
  (make-array +randomness-string-len+ :element-type 'character :adjustable nil))

(defun make-ulid-bytes-array ()
  (make-array +ulid-bytes-len+ :element-type '(unsigned-byte 8) :adjustable nil))

(defun make-ulid-string-array ()
  (make-array +ulid-string-len+ :element-type 'character :adjustable nil))

(defun timestamp->octets (timestamp &optional (octets (make-timestamp-bytes-array)) (index 0))
  "Convert a timestamp integer into a 6-byte array of octets."
  (check-type timestamp timestamp-integer)
  (ub48set/be octets index timestamp)
  octets)

(defun randomness->octets (randomness &optional (octets (make-randomness-bytes-array)) (index 0))
  "Convert a randomness integer into a 10-byte array of octets."
  (check-type randomness randomness-integer)
  (ub80set/be octets index randomness)
  octets)

(defun encode-timestamp-bytes (binary &optional
                                        (binary-offset 0)
                                        (timestamp-string-array (make-timestamp-string-array))
                                        (string-offset 0))
  "Encode a 6-byte timestamp into a 10-char string representation. The timestamp uses 48 bits,
taking 5 bits at a time, (crossing byte boundries when necessary) encoding into
a char from the base32 lookup table."
  ;; (check-type binary timestamp-byte-array)
  ;; (unless (= (length binary) +timestamp-bytes-len+)
  ;;   (error "timestamp value has to be exactly 6 bytes long."))
  (let ((lut +encode-lut+)
        (result timestamp-string-array))
    (setf (aref result (+ string-offset 0)) (aref lut (ash (logand (aref binary (+ binary-offset 0)) 224) -5)))
    (setf (aref result (+ string-offset 1)) (aref lut (logand (aref binary (+ binary-offset 0)) 31)))
    (setf (aref result (+ string-offset 2)) (aref lut (ash (logand (aref binary (+ binary-offset 1)) 248) -3)))
    (setf (aref result (+ string-offset 3)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 1)) 7) 2)
                                                              (ash (logand (aref binary (+ binary-offset 2)) 192) -6))))
    (setf (aref result (+ string-offset 4)) (aref lut (ash (logand (aref binary (+ binary-offset 2)) 62) -1)))
    (setf (aref result (+ string-offset 5)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 2)) 1) 4)
                                                              (ash (logand (aref binary (+ binary-offset 3)) 240) -4))))
    (setf (aref result (+ string-offset 6)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 3)) 15) 1)
                                                              (ash (logand (aref binary (+ binary-offset 4)) 128) -7))))
    (setf (aref result (+ string-offset 7)) (aref lut (ash (logand (aref binary (+ binary-offset 4)) 124) -2)))
    (setf (aref result (+ string-offset 8)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 4)) 3) 3)
                                                              (ash (logand (aref binary (+ binary-offset 5)) 224) -5))))
    (setf (aref result (+ string-offset 9)) (aref lut (logand (aref binary (+ binary-offset 5)) 31)))
    result))

(defun encode-timestamp (timestamp)
  "Encode a 48-bit timestamp into a 10-char string representation."
  (check-type timestamp timestamp-integer)
  (encode-timestamp-bytes (timestamp->octets timestamp +timestamp-bytes-len+)))

(defun encode-randomness-bytes (binary &optional
                                        (binary-offset 0)
                                        (randomness-string-array (make-randomness-string-array))
                                        (string-offset 0))
  "Encode a 10-byte random into a 16-char string representation. The random uses 80 bits,
taking 5 bits at a time, (crossing byte boundries when necessary) encoding into
a char from the base32 lookup table."
  ;; (check-type binary randomness-byte-array)
  ;; (unless (= (length binary) +randomness-bytes-len+)
  ;;   (error "Randomness value has to be exactly 10 bytes long"))
  (let ((lut +encode-lut+)
        (result randomness-string-array))
    (setf (aref result (+ string-offset 0)) (aref lut (ash (logand (aref binary (+ binary-offset 0)) 248) -3)))
    (setf (aref result (+ string-offset 1)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 0)) 7) 2)
                                                              (ash (logand (aref binary (+ binary-offset 1)) 192) -6))))
    (setf (aref result (+ string-offset 2)) (aref lut (ash (logand (aref binary (+ binary-offset 1)) 62) -1)))
    (setf (aref result (+ string-offset 3)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 1)) 1) 4)
                                                              (ash (logand (aref binary (+ binary-offset 2)) 240) -4))))
    (setf (aref result (+ string-offset 4)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 2)) 15) 1)
                                                              (ash (logand (aref binary (+ binary-offset 3)) 128) -7))))
    (setf (aref result (+ string-offset 5)) (aref lut (ash (logand (aref binary (+ binary-offset 3)) 124) -2)))
    (setf (aref result (+ string-offset 6)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 3)) 3) 3)
                                                              (ash (logand (aref binary (+ binary-offset 4)) 224) -5))))
    (setf (aref result (+ string-offset 7)) (aref lut (logand (aref binary (+ binary-offset 4)) 31)))
    (setf (aref result (+ string-offset 8)) (aref lut (ash (logand (aref binary (+ binary-offset 5)) 248) -3)))
    (setf (aref result (+ string-offset 9)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 5)) 7) 2)
                                                              (ash (logand (aref binary (+ binary-offset 6)) 192) -6))))
    (setf (aref result (+ string-offset 10)) (aref lut (ash (logand (aref binary (+ binary-offset 6)) 62) -1)))
    (setf (aref result (+ string-offset 11)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 6)) 1) 4)
                                                               (ash (logand (aref binary (+ binary-offset 7)) 240) -4))))
    (setf (aref result (+ string-offset 12)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 7)) 15) 1)
                                                               (ash (logand (aref binary (+ binary-offset 8)) 128) -7))))
    (setf (aref result (+ string-offset 13)) (aref lut (ash (logand (aref binary (+ binary-offset 8)) 124) -2)))
    (setf (aref result (+ string-offset 14)) (aref lut (logior (ash (logand (aref binary (+ binary-offset 8)) 3) 3)
                                                               (ash (logand (aref binary (+ binary-offset 9)) 224) -5))))
    (setf (aref result (+ string-offset 15)) (aref lut (logand (aref binary (+ binary-offset 9)) 31)))
    result))

(defun encode-randomness (random)
  "Encode a 80-bit random into a 16-char string representation."
  (check-type random randomness-integer)
  (encode-randomness-bytes (randomness->octets random)))

(defun encode-ulid-bytes (binary &optional
                                   (ulid-string-array (make-ulid-string-array)))

  (unless (= (length binary) +ulid-bytes-len+)
    (error 'type-error "ULID bytes has to be exactly 16 bytes long"))
  (encode-timestamp-bytes binary 0 ulid-string-array 0)
  (encode-randomness-bytes binary +timestamp-bytes-len+ ulid-string-array +timestamp-string-len+)
  ulid-string-array)

(defun decode-timestamp-to-bytes (encoded &optional
                                            (timestamp-bytes-array (make-timestamp-bytes-array))
                                            (binary-offset 0))
  "Decode a 10-char string representation of a timestamp into a 6-byte array."
  (if (not (= (length encoded) +timestamp-string-len+))
      (error 'type-error "ULID encoded timestamp has to be exactly 10 characters long."))
  (let* ((lut +decode-lut+)
         (enc-bytes (babel:string-to-octets encoded :encoding :ascii))
         (result timestamp-bytes-array))
    (setf (aref result (+ binary-offset 0)) (logand (logior (ash (aref lut (aref enc-bytes 0)) 5)
                                                            (aref lut (aref enc-bytes 1)))
                                                    #xFF))
    (setf (aref result (+ binary-offset 1)) (logand (logior (ash (aref lut (aref enc-bytes 2)) 3)
                                                            (ash (aref lut (aref enc-bytes 3)) -2))
                                                    #xFF))
    (setf (aref result (+ binary-offset 2)) (logand (logior (logior (ash (aref lut (aref enc-bytes 3)) 6)
                                                                    (ash (aref lut (aref enc-bytes 4)) 1))
                                                            (ash (aref lut (aref enc-bytes 5)) -4))
                                                    #xFF))
    (setf (aref result (+ binary-offset 3)) (logand (logior (ash (aref lut (aref enc-bytes 5)) 4)
                                                            (ash (aref lut (aref enc-bytes 6)) -1))
                                                    #xFF))
    (setf (aref result (+ binary-offset 4)) (logand (logior (logior (ash (aref lut (aref enc-bytes 6)) 7)
                                                                    (ash (aref lut (aref enc-bytes 7)) 2))
                                                            (ash (aref lut (aref enc-bytes 8)) -3))
                                                    #xFF))
    (setf (aref result (+ binary-offset 5)) (logand (logior (ash (aref lut (aref enc-bytes 8)) 5)
                                                            (aref lut (aref enc-bytes 9)))
                                                    #xFF))
    result))

(defun decode-randomness-to-bytes (encoded &optional
                                            (randomness-bytes-array (make-randomness-bytes-array))
                                            (binary-offset 0))
  "Decode a 16-char string representation of randomness into a 10-byte array."
  (unless (= (length encoded) +randomness-string-len+)
    (error 'type-error "ULID encoded randomness has to be exactly 16 characters long."))
  (let* ((lut +decode-lut+)
         (enc-bytes (babel:string-to-octets encoded :encoding :ascii))
         (result randomness-bytes-array))
    (setf (aref result (+ binary-offset 0)) (logand (logior (ash (aref lut (aref enc-bytes 0)) 3)
                                                            (ash (aref lut (aref enc-bytes 1)) -2))
                                                    #xFF))
    (setf (aref result (+ binary-offset 1)) (logand (logior (logior (ash (aref lut (aref enc-bytes 1)) 6)
                                                                    (ash (aref lut (aref enc-bytes 2)) 1))
                                                            (ash (aref lut (aref enc-bytes 3)) -4))
                                                    #xFF))
    (setf (aref result (+ binary-offset 2)) (logand (logior (ash (aref lut (aref enc-bytes 3)) 4)
                                                            (ash (aref lut (aref enc-bytes 4)) -1))
                                                    #xFF))
    (setf (aref result (+ binary-offset 3)) (logand (logior (logior (ash (aref lut (aref enc-bytes 4)) 7)
                                                                    (ash (aref lut (aref enc-bytes 5)) 2))
                                                            (ash (aref lut (aref enc-bytes 6)) -3))
                                                    #xFF))
    (setf (aref result (+ binary-offset 4)) (logand (logior (ash (aref lut (aref enc-bytes 6)) 5)
                                                            (aref lut (aref enc-bytes 7)))
                                                    #xFF))
    (setf (aref result (+ binary-offset 5)) (logand (logior (ash (aref lut (aref enc-bytes 8)) 3)
                                                            (ash (aref lut (aref enc-bytes 9)) -2))
                                                    #xFF))
    (setf (aref result (+ binary-offset 6)) (logand (logior (logior (ash (aref lut (aref enc-bytes 9)) 6)
                                                                    (ash (aref lut (aref enc-bytes 10)) 1))
                                                            (ash (aref lut (aref enc-bytes 11)) -4))
                                                    #xFF))
    (setf (aref result (+ binary-offset 7)) (logand (logior (ash (aref lut (aref enc-bytes 11)) 4)
                                                            (ash (aref lut (aref enc-bytes 12)) -1))
                                                    #xFF))
    (setf (aref result (+ binary-offset 8)) (logand (logior (logior (ash (aref lut (aref enc-bytes 12)) 7)
                                                                    (ash (aref lut (aref enc-bytes 13)) 2))
                                                            (ash (aref lut (aref enc-bytes 14)) -3))
                                                    #xFF))
    (setf (aref result (+ binary-offset 9)) (logand (logior (ash (aref lut (aref enc-bytes 14)) 5)
                                                            (aref lut (aref enc-bytes 15)))
                                                    #xFF))
    result))

(defun decode-ulid-to-bytes (encoded &optional
                                        (ulid-bytes-array (make-ulid-bytes-array)))
  "Decode a 26-char string representation of a ULID into a 16-byte array."
  (if (not (= (length encoded) +ulid-string-len+))
      (error "Encoded ULID has to be exactly 26 characters long."))
  (let* ((timestamp-section (subseq encoded 0 +timestamp-string-len+))
         (randomness-section (subseq encoded +timestamp-string-len+)))
    (decode-timestamp-to-bytes timestamp-section ulid-bytes-array 0)
    (decode-randomness-to-bytes randomness-section ulid-bytes-array +timestamp-bytes-len+)
    ulid-bytes-array))

(defun decode-to-values (encoded)
  "Decode ENCODED. ENCODED is either a 26-char string representation of a ULID,
or a 16 byte representation of a ULID.
 returning integer values: timestamp,randomness."
  (etypecase encoded
    (string (let ((ulid-bytes (decode-ulid-to-bytes encoded)))
              (values (ub48ref/be ulid-bytes 0)
                      (ub80ref/be ulid-bytes +timestamp-bytes-len+))))
    (ulid-byte-array (values (ub48ref/be encoded 0)
                             (ub80ref/be encoded +timestamp-bytes-len+)))))
