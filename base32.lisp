;; (ql:quickload '(:serapeum :cl+ssl :secure-random))

(defpackage :ulid
  (:use :cl)
  (:import-from #:alexandria
                #:iota)
  (:import-from #:serapeum
                #:get-unix-time)
  (:export
   #:ulid))

(in-package :ulid)

(defconstant +timestamp-len+ 6)
(defconstant +randomness-len+ 10)
(defconstant +bytes-len+ (+ +timestamp-len+ +randomness-len+))

(defconstant +timestamp-repr-len+ 10)
(defconstant +randomness-repr-len+ 16)
(defconstant +repr-len+ (+ +timestamp-repr-len+ +randomness-repr-len+))

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

(defun check-decode-array-is-same ()
  (let ((py-decode-array (ulid-py::get-decode-array)))
    (loop for i from 0 to (1- (length py-decode-array))
          do (assert (= (aref py-decode-array i)
                        (aref *decode* i))))))
;; (check-decode-array-is-same)
;; (aref *encode* 10)

;; (reduce (lambda (x y) (concatenate 'string x (string y))) result)
;; (reduce (lambda (x y) (concatenate 'string x (string y))) result)

(defun encode-timestamp (binary)
  (unless (= (length binary) +timestamp-len+)
    (error 'value-error :format-control "timestamp value has to be exactly 6 bytes long."))
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
    (values (coerce result 'string) result)
    ))

;; (encode-timestamp (vector 255 255 255 255 255 255))
;; (encode-timestamp (crypto:random-data 6))
;; (length (vector 255 255 255 255 255 255))
