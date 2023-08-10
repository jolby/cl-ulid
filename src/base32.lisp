(in-package :ulid)

(defconstant +time-max+ (1- (expt 2 48)))
(defconstant +random-max+ (1- (expt 2 80)))
(defconstant +timestamp-len+ 6)
(defconstant +randomness-len+ 10)
(defconstant +bytes-len+ (+ +timestamp-len+ +randomness-len+))

(defconstant +timestamp-repr-len+ 10)
(defconstant +randomness-repr-len+ 16)
(defconstant +repr-len+ (+ +timestamp-repr-len+ +randomness-repr-len+))

(deftype timestamp-byte-array () '(simple-array (unsigned-byte 8) (#.+timestamp-len+)))
(deftype randomness-byte-array () '(simple-array (unsigned-byte 8) (#.+randomness-len+)))
(deftype ulid-byte-array () '(simple-array (unsigned-byte 8) (#.+bytes-len+)))

(deftype timestamp-integer () '(unsigned-byte 48))
(deftype randomness-integer () '(unsigned-byte 80))

;;;; Lookup tables for encoding and decoding
;;;; The encoding and decoding arithmetics are based on the implementation of RobThree
;;;; https://github.com/RobThree/NUlid/blob/89f5a9fc827d191ae5adafe42547575ed3a47723/NUlid/Ulid.cs#L168

(defparameter *encode* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")

(defparameter *decode*
  #(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
  255 255 255 255 255 255 255 255 255 255 0 1 2 3 4 5 6 7 8 9 255 255 255 255
  255 255 255 10 11 12 13 14 15 16 17 255 18 19 255 20 21 255 22 23 24 25 26
  255 27 28 29 30 31 255 255 255 255 255 255 10 11 12 13 14 15 16 17 255 18 19
  255 20 21 255 22 23 24 25 26 255 27 28 29 30 31 255 255 255 255 255 255 255)
  "Char to index lookup array for massive speedup since we can find a char's
 index in O(1). We use 255 as 'sentinel' value for invalid indexes. ")


(defun encode-timestamp-bytes (binary)
  "Encode a 6-byte timestamp into a 10-char string representation. The timestamp uses 48 bits,
taking 5 bits at a time, (crossing byte boundries when necessary) encoding into
a char from the base32 lookup table."
  (check-type binary timestamp-byte-array)
  (unless (= (length binary) +timestamp-len+)
    (error "timestamp value has to be exactly 6 bytes long."))
  (let ((lut *encode*)
        (result (make-array 10)))
    (setf (aref result 0) (aref lut (ash (logand (aref binary 0) 224) -5)))
    (setf (aref result 1) (aref lut (logand (aref binary 0) 31)))
    (setf (aref result 2) (aref lut (ash (logand (aref binary 1) 248) -3)))
    (setf (aref result 3) (aref lut (logior (ash (logand (aref binary 1) 7) 2)
                                            (ash (logand (aref binary 2) 192) -6))))
    (setf (aref result 4) (aref lut (ash (logand (aref binary 2) 62) -1)))
    (setf (aref result 5) (aref lut (logior (ash (logand (aref binary 2) 1) 4)
                                            (ash (logand (aref binary 3) 240) -4))))
    (setf (aref result 6) (aref lut (logior (ash (logand (aref binary 3) 15) 1)
                                            (ash (logand (aref binary 4) 128) -7))))
    (setf (aref result 7) (aref lut (ash (logand (aref binary 4) 124) -2)))
    (setf (aref result 8) (aref lut (logior (ash (logand (aref binary 4) 3) 3)
                                            (ash (logand (aref binary 5) 224) -5))))
    (setf (aref result 9) (aref lut (logand (aref binary 5) 31)))
    (values (coerce result 'string) result)))

(defun encode-timestamp (timestamp)
  "Encode a 48-bit timestamp into a 10-char string representation."
  (check-type timestamp timestamp-integer)
  (encode-timestamp-bytes (int->octets timestamp +timestamp-len+)))

(defun encode-randomness-bytes (binary)
  "Encode a 10-byte random into a 16-char string representation. The random uses 80 bits,
taking 5 bits at a time, (crossing byte boundries when necessary) encoding into
a char from the base32 lookup table."
  (check-type binary randomness-byte-array)
  (unless (= (length binary) +randomness-len+)
    (error "Randomness value has to be exactly 10 bytes long"))
  (let ((lut *encode*)
        (result (make-array 16)))
    (setf (aref result 0) (aref lut (ash (logand (aref binary 0) 248) -3)))
    (setf (aref result 1) (aref lut (logior (ash (logand (aref binary 0) 7) 2)
                                            (ash (logand (aref binary 1) 192) -6))))
    (setf (aref result 2) (aref lut (ash (logand (aref binary 1) 62) -1)))
    (setf (aref result 3) (aref lut (logior (ash (logand (aref binary 1) 1) 4)
                                            (ash (logand (aref binary 2) 240) -4))))
    (setf (aref result 4) (aref lut (logior (ash (logand (aref binary 2) 15) 1)
                                            (ash (logand (aref binary 3) 128) -7))))
    (setf (aref result 5) (aref lut (ash (logand (aref binary 3) 124) -2)))
    (setf (aref result 6) (aref lut (logior (ash (logand (aref binary 3) 3) 3)
                                            (ash (logand (aref binary 4) 224) -5))))
    (setf (aref result 7) (aref lut (logand (aref binary 4) 31)))
    (setf (aref result 8) (aref lut (ash (logand (aref binary 5) 248) -3)))
    (setf (aref result 9) (aref lut (logior (ash (logand (aref binary 5) 7) 2)
                                            (ash (logand (aref binary 6) 192) -6))))
    (setf (aref result 10) (aref lut (ash (logand (aref binary 6) 62) -1)))
    (setf (aref result 11) (aref lut (logior (ash (logand (aref binary 6) 1) 4)
                                             (ash (logand (aref binary 7) 240) -4))))
    (setf (aref result 12) (aref lut (logior (ash (logand (aref binary 7) 15) 1)
                                             (ash (logand (aref binary 8) 128) -7))))
    (setf (aref result 13) (aref lut (ash (logand (aref binary 8) 124) -2)))
    (setf (aref result 14) (aref lut (logior (ash (logand (aref binary 8) 3) 3)
                                             (ash (logand (aref binary 9) 224) -5))))
    (setf (aref result 15) (aref lut (logand (aref binary 9) 31)))
    (values (coerce result 'string) result)))

(defun encode-randomness (random)
  "Encode a 80-bit random into a 16-char string representation."
  (check-type random randomness-integer)
  (encode-randomness-bytes (int->octets random)))

(defun encode-timestamp-and-randomness-bytes (timestamp-bytes random-bytes)
  (concatenate 'string
               (encode-timestamp-bytes timestamp-bytes)
               (encode-randomness-bytes random-bytes)))

(defun encode-bytes (binary)
  (unless (= (length binary) +bytes-len+)
    (error 'type-error "ULID bytes has to be exactly 16 bytes long"))
  (concatenate 'string
               (encode-timestamp-bytes (subseq binary 0 +timestamp-len+))
               (encode-randomness-bytes (subseq binary +timestamp-len+))))

(defun decode-timestamp-to-bytes (encoded)
  "Decode a 10-char string representation of a timestamp into a 6-byte array."
  (if (not (= (length encoded) +timestamp-repr-len+))
      (error 'type-error "ULID encoded timestamp has to be exactly 10 characters long."))
  (let* ((lut *decode*)
         (enc-bytes (babel:string-to-octets encoded :encoding :ascii))
         (result (make-array 6)))
    (setf (aref result 0) (logand (logior (ash (aref lut (aref enc-bytes 0)) 5)
                                          (aref lut (aref enc-bytes 1)))
                                  #xFF))
    (setf (aref result 1) (logand (logior (ash (aref lut (aref enc-bytes 2)) 3)
                                          (ash (aref lut (aref enc-bytes 3)) -2))
                                  #xFF))
    (setf (aref result 2) (logand (logior (logior (ash (aref lut (aref enc-bytes 3)) 6)
                                                (ash (aref lut (aref enc-bytes 4)) 1))
                                           (ash (aref lut (aref enc-bytes 5)) -4))
                                  #xFF))
    (setf (aref result 3) (logand (logior (ash (aref lut (aref enc-bytes 5)) 4)
                                          (ash (aref lut (aref enc-bytes 6)) -1))
                                  #xFF))
    (setf (aref result 4) (logand (logior (logior (ash (aref lut (aref enc-bytes 6)) 7)
                                                (ash (aref lut (aref enc-bytes 7)) 2))
                                           (ash (aref lut (aref enc-bytes 8)) -3))
                                  #xFF))
    (setf (aref result 5) (logand (logior (ash (aref lut (aref enc-bytes 8)) 5)
                                          (aref lut (aref enc-bytes 9)))
                                  #xFF))
    result))

(defun decode-randomness-to-bytes (encoded)
  "Decode a 16-char string representation of randomness into a 10-byte array."
  (unless (= (length encoded) +randomness-repr-len+)
      (error 'type-error "ULID encoded randomness has to be exactly 16 characters long."))
  (let* ((lut *decode*)
         (enc-bytes (babel:string-to-octets encoded :encoding :ascii))
         (result (make-array 10)))
    (setf (aref result 0) (logand (logior (ash (aref lut (aref enc-bytes 0)) 3)
                                          (ash (aref lut (aref enc-bytes 1)) -2))
                                  #xFF))
    (setf (aref result 1) (logand (logior (logior (ash (aref lut (aref enc-bytes 1)) 6)
                                                (ash (aref lut (aref enc-bytes 2)) 1))
                                           (ash (aref lut (aref enc-bytes 3)) -4))
                                  #xFF))
    (setf (aref result 2) (logand (logior (ash (aref lut (aref enc-bytes 3)) 4)
                                          (ash (aref lut (aref enc-bytes 4)) -1))
                                  #xFF))
    (setf (aref result 3) (logand (logior (logior (ash (aref lut (aref enc-bytes 4)) 7)
                                                (ash (aref lut (aref enc-bytes 5)) 2))
                                           (ash (aref lut (aref enc-bytes 6)) -3))
                                  #xFF))
    (setf (aref result 4) (logand (logior (ash (aref lut (aref enc-bytes 6)) 5)
                                          (aref lut (aref enc-bytes 7)))
                                  #xFF))
    (setf (aref result 5) (logand (logior (ash (aref lut (aref enc-bytes 8)) 3)
                                          (ash (aref lut (aref enc-bytes 9)) -2))
                                  #xFF))
    (setf (aref result 6) (logand (logior (logior (ash (aref lut (aref enc-bytes 9)) 6)
                                                (ash (aref lut (aref enc-bytes 10)) 1))
                                           (ash (aref lut (aref enc-bytes 11)) -4))
                                  #xFF))
    (setf (aref result 7) (logand (logior (ash (aref lut (aref enc-bytes 11)) 4)
                                          (ash (aref lut (aref enc-bytes 12)) -1))
                                  #xFF))
    (setf (aref result 8) (logand (logior (logior (ash (aref lut (aref enc-bytes 12)) 7)
                                                (ash (aref lut (aref enc-bytes 13)) 2))
                                           (ash (aref lut (aref enc-bytes 14)) -3))
                                  #xFF))
    (setf (aref result 9) (logand (logior (ash (aref lut (aref enc-bytes 14)) 5)
                                          (aref lut (aref enc-bytes 15)))
                                  #xFF))
    result))

(defun decode-to-bytes (encoded)
  "Decode a 26-char string representation of a ULID into a 16-byte array."
  (if (not (= (length encoded) +repr-len+))
      (error "Encoded ULID has to be exactly 26 characters long."))
  (let* ((timestamp-section (subseq encoded 0 +timestamp-repr-len+))
         (randomness-section (subseq encoded +timestamp-repr-len+)))
    (concatenate 'vector
                 (decode-timestamp timestamp-section)
                 (decode-randomness randomness-section))))

(defun decode-to-values (encoded)
  "Decode a 26-char string representation of a ULID, returning integer values: timestamp, randomness."
  )

(defun decode-to-ulid (encoded)
  "Decode a 26-char string representation of a ULID into a ULID struct."
  )
