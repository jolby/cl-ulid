(in-package :ulid/tests)

(def-suite ulid/tests-suite)

(in-suite ulid/tests-suite)

(test ulid/tests-suite-exists
  (is-true t))

(defun vec-elts-equal (v1 v2)
  (loop for i from 0 below (length v1)
        always (= (aref v1 i) (aref v2 i))))

(test valid-time-and-random-integers )

(test valid-base32-encoder-input-1
  ;; one byte too many ==> error
  (signals simple-error (encode-timestamp (vector 255 255 255 255 255 255 125)))
  (finishes (encode-timestamp (vector 255 255 255 255 255 124)))
  (signals simple-error (encode-randomness (vector 255 255 255 255 255 255 255 255 255 125 125))
  (finishes (encode-randomness (vector 255 255 255 255 255 255 255 255 255 125)))))

(test round-trip.smoke-1
  (let ((all-passed 't))
    (loop :for i :below 1000
          :do (let* ((rand-16-bytes (funcall *random-bytes-fn* ulid::+bytes-len+))
                     (lisp-encode (encode rand-16-bytes))
                     (lisp-decode (decode lisp-encode)))
                (unless (vec-elts-equal rand-16-bytes lisp-decode)
                  (setf all-passed nil))))
    (format t "all-passed: ~a~%" all-passed)
    (is-true all-passed)))

(test timestamp-retreival
  (let* ((ts (ulid::get-unix-time-ms))
         (ulid-str (ulid:make-ulid-string ts))
         (ulid (ulid:make-ulid ts)))
    (is (= ts (ulid:ulid-timestamp ulid-str) (ulid:ulid-timestamp ulid)))))

;; (ql:quickload '(:ulid/tests))
;; (run! 'ulid/tests-suite-exists)
;; (run! 'ulid/tests::valid-base32-encoder-input-1)
;; (run! 'ulid/tests::round-trip.smoke-1)
;; (run! 'ulid/tests::timestamp-retreival)
;; (run! 'ulid/tests-suite)


;; (encode-timestamp (vector 255 255 255 255 255 255))
;; (encode-timestamp (vector 255 255 255 255 255 255 125)) ==> error
;; (encode-randomness (vector 255 255 255 255 255 255 255 255 255 125))
;; (encode-timestamp (crypto:random-data 6))
;; (defparameter *rand-6* (crypto:random-data 6))
;; (defparameter *rand-6-enc* (encode-timestamp *rand-6*))
;; (coerce *rand-6-enc* '(vector (unsigned-byte 8)))
;; (babel:string-to-octets *rand-6-enc* :encoding :ascii)
;; (length (vector 255 255 255 255 255 255))

#+(or)(let* ((encode-ts (encode-timestamp (crypto:random-data 6)))
             (decode-ts (decode-timestamp encode-ts)))
         (assert (equal (coerce (crypto:random-data 6) 'string)
                        (coerce decode-ts 'string))))
#+(or)(progn
        (loop :for i :below 1000
              :do (let* ((rand-6-bytes (crypto:random-data 6))
                         (lisp-encode (encode-timestamp rand-6-bytes))
                         (py-encode (ulid-py::ulid-py-encode-timestamp rand-6-bytes)))
                    (assert (equal lisp-encode py-encode))))
        (log:info "encode-timestamp test passed."))

#+(or)(progn
        (loop :for i :below 1000
              :do (let* ((rand-10-bytes (crypto:random-data 10))
                         (lisp-encode (encode-randomness rand-10-bytes))
                         (py-encode (ulid-py::ulid-py-encode-randomness rand-10-bytes)))
                    (assert (equal lisp-encode py-encode))))
        (log:info "encode-randomness test passed."))

#+(or)(progn
        (loop :for i :below 1000
              :do (let* ((rand-16-bytes (crypto:random-data 16))
                         (lisp-encode (encode rand-16-bytes))
                         (py-encode (ulid-py::ulid-py-encode rand-16-bytes)))
                    (assert (equal lisp-encode py-encode))))
        (log:info "encode test passed."))

#+(or)(progn
        (loop :for i :below 1000
              :do (let* ((rand-16-bytes (crypto:random-data 16))
                         (lisp-encode (encode rand-16-bytes))
                         (lisp-decode (decode lisp-encode)))
                    (assert (vec-elts-equal rand-16-bytes lisp-decode))))
        (log:info "encode/decode test passed."))
